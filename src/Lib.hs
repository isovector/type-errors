
module Lib
  ( -- * Generating Error Messages
    ErrorMessage (..)
  , PrettyPrintList
  , ShowTypeQuoted

    -- * Emitting Error Messages
  , TypeError
  , DelayError
  , DelayErrorFcf
  , NoError
  , NoErrorFcf

  -- * Observing Stuckness
  , IfStuck
  , WhenStuck
  , UnlessStuck

    -- * Observing Phantomness
  , PHANTOM
  , UnlessPhantom
  , UnlessPhantomFcf

    -- * Performing Type Substitutions
  , Subst
  , VAR
  , SubstVar

    -- * Working With Fcfs
  , Exp
  , Eval
  , Pure
  ) where

import Fcf
import Data.Kind
import GHC.TypeLits
import Data.Coerce

-- $setup
-- >>> :m +Data.Kind
-- >>> :m +Data.Proxy
-- >>> import GHC.Generics (Generic (..))
-- >>> :def! show_error (\msg -> pure $ "let foo :: DelayError (" ++ msg ++ ") => (); foo = (); in foo")
-- >>> :{
-- data HasNoRep = HasNoRep
-- :}


------------------------------------------------------------------------------
-- | Pretty print a list.
--
-- >>> :show_error PrettyPrintList '[Bool]
-- ...
-- ... 'Bool'
-- ...
--
-- >>> :show_error PrettyPrintList '[1, 2]
-- ...
-- ... '1', and '2'
-- ...
--
-- >>> :show_error PrettyPrintList '["hello", "world", "cool"]
-- ...
-- ... "hello", "world", and "cool"
-- ...
type family PrettyPrintList (vs :: [k]) :: ErrorMessage where
  PrettyPrintList '[]       = 'Text ""
  PrettyPrintList '[a]      = ShowTypeQuoted a
  PrettyPrintList '[a, b]   = ShowTypeQuoted a ':<>: 'Text ", and " ':<>: ShowTypeQuoted b
  PrettyPrintList (a ': vs) = ShowTypeQuoted a ':<>: 'Text ", " ':<>: PrettyPrintList vs


------------------------------------------------------------------------------
-- | Like 'ShowType', but quotes the type if necessary.
--
-- >>> :show_error ShowTypeQuoted Int
-- ...
-- ... 'Int'
-- ...
--
-- >>> :show_error ShowTypeQuoted "symbols aren't quoted"
-- ...
-- ... "symbols aren't quoted"
-- ...
type family ShowTypeQuoted (t :: k) :: ErrorMessage where
  ShowTypeQuoted (t :: Symbol) = 'ShowType t
  ShowTypeQuoted t             = 'Text "'" ':<>: 'ShowType t ':<>: 'Text "'"


------------------------------------------------------------------------------
-- | Error messages produced via 'TypeError' are often /too strict/, and will
-- be emitted sooner than you'd like. The solution is to use 'DelayError',
-- which will switch the error messages to being consumed lazily.
--
-- >>> :{
-- foo :: TypeError ('Text "Too eager; prevents compilation") => ()
-- foo = ()
-- :}
-- ...
-- ... Too eager; prevents compilation
-- ...
--
-- >>> :{
-- foo :: DelayError ('Text "Lazy; emitted on use") => ()
-- foo = ()
-- :}
--
-- >>> foo
-- ...
-- ... Lazy; emitted on use
-- ...
type DelayError err = Eval (DelayErrorFcf err)


------------------------------------------------------------------------------
-- | Like 'DelayError', but implemented as a first-class family.
-- 'DelayErrorFcf' is useful when used as the last argument to 'IfStuck' and
-- 'UnlessStuck'.
data DelayErrorFcf :: ErrorMessage -> Exp k
type instance Eval (DelayErrorFcf a) = TypeError a


------------------------------------------------------------------------------
-- | A helper definition that /doesn't/ emit a type error. This is
-- occassionally useful to leave as the residual constraint in 'IfStuck' when
-- you only want to observe if an expression /isn't/ stuck.
type NoError = (() :: Constraint)


------------------------------------------------------------------------------
-- | Like 'NoError', but implemented as a first-class family.  'NoErrorFcf' is
-- useful when used as the last argument to 'IfStuck' and 'UnlessStuck'.
type NoErrorFcf = Pure NoError


------------------------------------------------------------------------------
-- | @'IfStuck' expr b c@ leaves @b@ in the residual constraints whenever
-- @expr@ is stuck, otherwise it 'Eval'uates @c@.
--
--
type family IfStuck (expr :: k) (b :: k1) (c :: Exp k1) :: k1 where
  IfStuck (_ AnythingOfAnyKind) b c = b
  IfStuck a                     b c = Eval c

data AnythingOfAnyKind


------------------------------------------------------------------------------
-- |
--
-- >>> :{
-- observe_no_rep
--     :: WhenStuck
--          (Rep t)
--          (DelayError ('Text "No Rep instance for " ':<>: ShowTypeQuoted t))
--     => t
--     -> ()
-- observe_no_rep _ = ()
-- :}
--
-- >>> observe_no_rep HasNoRep
-- ...
-- ... No Rep instance for 'HasNoRep'
-- ...
--
-- >>> observe_no_rep True
-- ()
type WhenStuck expr b   = IfStuck expr b NoErrorFcf


------------------------------------------------------------------------------
-- | Like 'IfStuck', but leaves no residual constraint when @expr@ is stuck.
-- This can be used to ensure an expression /isn't/ stuck before analyzing it
-- further.
--
-- See the example under 'UnlessPhantomFcf' for an example of this use-case.
type UnlessStuck expr c = IfStuck expr NoError c


------------------------------------------------------------------------------
-- | A meta-variable for marking which argument should be a phantom when
-- working with 'UnlessPhantom' and 'UnlessPhantomFcf'.
--
-- 'PHANTOM' is polykinded and can be used in several settings.
--
-- See 'UnlessPhantomFcf' for examples.
type PHANTOM = VAR

type UnlessPhantom exp err = Eval (UnlessPhantomFcf exp err)

------------------------------------------------------------------------------
-- |
--
-- >>> :{
-- observe_phantom
--     :: UnlessStuck
--          f
--          (UnlessPhantomFcf
--            (f PHANTOM)
--            ('Text "It's not phantom!"))
--     => f p
--     -> ()
-- observe_phantom _ = ()
-- :}
--
-- >>> observe_phantom Proxy
-- ()
--
-- >>> observe_phantom (Just 5)
-- ...
-- ... It's not phantom!
-- ...
--
-- >>> observe_phantom
-- ...
-- ... No instance for (Show ...
-- ...
data UnlessPhantomFcf :: k -> ErrorMessage -> Exp Constraint
type instance Eval (UnlessPhantomFcf exp err) =
  Coercible (SubstVar exp Stuck)
            (SubstVar exp (DelayError err))


------------------------------------------------------------------------------
-- |
--
-- >>> :kind! Subst (Either Int Int) Int Bool
-- ...
-- = Either Bool Bool
--
-- >>> :kind! Subst (Either Int Bool) Int [Char]
-- ...
-- = Either [Char] Bool
--
-- >>> :kind! Subst (Either Int Bool) Either (->)
-- ...
-- = Int -> Bool
type family Subst (e :: k1) (var :: k2) (sub :: k2) :: k1 where
  Subst var var sub   = sub
  Subst (a b) var sub = Subst a var sub (Subst b var sub)
  Subst a var sub     = a

data Var
type family SubMe :: Type -> k

------------------------------------------------------------------------------
-- | 'VAR' is a meta-varaible which marks a substitution in 'SubstVar'. The
-- result of @'SubstVar' expr val@ is @expr[val/'VAR']@.
--
-- 'VAR' is polykinded and can be used in several settings.
--
-- See 'SubstVar' for examples.
type VAR = SubMe Var


------------------------------------------------------------------------------
-- |
--
-- >>> :kind! SubstVar (Either VAR Bool) [Char]
-- ...
-- = Either [Char] Bool
--
-- >>> :kind! SubstVar (VAR Int Bool :: Type) (->)
-- ...
-- = Int -> Bool
type family SubstVar (e :: k1) (r :: k2) :: k1 where
  SubstVar (_ Var) r = r
  SubstVar (a b) r   = SubstVar a r (SubstVar b r)
  SubstVar a r       = a

