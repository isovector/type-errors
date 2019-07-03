module Type.Errors
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

    -- * Running Magic
  , te

    -- * Observing Phantomness
  , PHANTOM
  , UnlessPhantom

    -- * Performing Type Substitutions
  , Subst
  , VAR
  , SubstVar

    -- * Working With Fcfs
  , Exp
  , Eval
  , Pure
  ) where

import           Control.Applicative
import           Data.Coerce
import           Data.Generics
import           Data.Kind
import           Fcf
import           GHC.TypeLits
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH hiding (Type, Exp)



-- $setup
-- >>> :m +Data.Kind
-- >>> :m +Data.Proxy
-- >>> import GHC.Generics (Generic (..))
-- >>> :def! show_error (\msg -> pure $ "let foo :: DelayError (" ++ msg ++ ") => (); foo = (); in foo")
-- >>> :def! eval_error (\msg -> pure $ "let foo :: (" ++ msg ++ ") => (); foo = (); in foo")
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
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
type DelayError err = Eval (DelayErrorFcf err)


------------------------------------------------------------------------------
-- | Like 'DelayError', but implemented as a first-class family.
-- 'DelayErrorFcf' is useful when used as the last argument to 'IfStuck' and
-- 'UnlessStuck'.
--
-- @since 0.1.0.0
data DelayErrorFcf :: ErrorMessage -> Exp k
type instance Eval (DelayErrorFcf a) = TypeError a


------------------------------------------------------------------------------
-- | A helper definition that /doesn't/ emit a type error. This is
-- occassionally useful to leave as the residual constraint in 'IfStuck' when
-- you only want to observe if an expression /isn't/ stuck.
--
-- @since 0.1.0.0
type NoError = (() :: Constraint)


------------------------------------------------------------------------------
-- | Like 'NoError', but implemented as a first-class family.  'NoErrorFcf' is
-- useful when used as the last argument to 'IfStuck' and 'UnlessStuck'.
--
-- @since 0.1.0.0
type NoErrorFcf = Pure NoError


------------------------------------------------------------------------------
-- | @'IfStuck' expr b c@ leaves @b@ in the residual constraints whenever
-- @expr@ is stuck, otherwise it 'Eval'uates @c@.
--
-- Often you want to leave a 'DelayError' in @b@ in order to report an error
-- when @expr@ is stuck.
--
-- The @c@ parameter is a first-class family, which allows you to perform
-- arbitrarily-complicated type-level computations whenever @expr@ isn't stuck.
-- For example, you might want to produce a typeclass 'Constraint' here.
-- Alternatively, you can nest calls to 'IfStuck' in order to do subsequent
-- processing.
--
-- This is a generalization of <https://kcsongor.github.io/ kcsongor>'s @Break@
-- machinery described in
-- <https://kcsongor.github.io/report-stuck-families/ detecting the undetectable>.
--
-- @since 0.1.0.0
type family IfStuck (expr :: k) (b :: k1) (c :: Exp k1) :: k1 where
  -- The type pattern @_ Foo@ is interpretered by the compiler as being of any
  -- kind. This is great and exactly what we want here, except that things like
  -- @forall s. Maybe s@ will get stuck here.
  --
  -- So instead, we just propagate out 100 of these type variables and assume
  -- that 100 type variables ought to be enough for anyone.
  IfStuck (_ AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind AnythingOfAnyKind AnythingOfAnyKind
             AnythingOfAnyKind) b c = b
  IfStuck a                     b c = Eval c

data AnythingOfAnyKind


------------------------------------------------------------------------------
-- | Like 'IfStuck', but specialized to the case when you don't want to do
-- anything if @expr@ isn't stuck.
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
--
-- @since 0.1.0.0
type WhenStuck expr b   = IfStuck expr b NoErrorFcf


------------------------------------------------------------------------------
-- | Like 'IfStuck', but leaves no residual constraint when @expr@ is stuck.
-- This can be used to ensure an expression /isn't/ stuck before analyzing it
-- further.
--
-- See the example under 'UnlessPhantomFcf' for an example of this use-case.
--
-- @since 0.1.0.0
type UnlessStuck expr c = IfStuck expr NoError c


------------------------------------------------------------------------------
-- |
te :: Q TH.Type -> Q TH.Type
te = liftA parseSubst


replaceWhen :: Data a => TH.Type -> TH.Type -> a -> a
replaceWhen a b = everywhere $ mkT $ \case
  x | x == a -> b
  x -> x


parseSubst :: TH.Type -> TH.Type
parseSubst (ForallT _ _ t) = parseSubst t
parseSubst (ConT phantom `AppT` expr `AppT` msg)
  | phantom == ''UnlessPhantom =
    ConT ''Coercible
      `AppT` (replaceWhen (ConT ''PHANTOM) (ConT ''Stuck) expr)
      `AppT` (replaceWhen (ConT ''PHANTOM)
                          (ConT ''DelayError `AppT` msg)
                          expr)
parseSubst (ConT subst `AppT` t `AppT` a `AppT` b)
  | subst == ''Subst = replaceWhen a b t
parseSubst (ConT subst `AppT` t `AppT` b)
  | subst == ''SubstVar = replaceWhen (ConT ''VAR) b t
parseSubst a = a



------------------------------------------------------------------------------
-- | A meta-variable for marking which argument should be a phantom when
-- working with 'UnlessPhantom' and 'UnlessPhantomFcf'.
--
-- 'PHANTOM' is polykinded and can be used in several settings.
--
-- See 'UnlessPhantom' for examples.
--
-- @since 0.1.0.0
type family PHANTOM :: k


------------------------------------------------------------------------------
-- | @'UnlessPhantom' expr err@ determines if the type described by @expr@
-- is phantom in the variables marked via 'PHANTOM'. If it's not, it produces
-- the error message @err@.
--
-- For example, consider the definition:
--
-- >>> :{
-- data Qux a b = Qux b
-- :}
--
-- which is phantom in @a@:
--
-- >>> :eval_error $(te[t| UnlessPhantom (Qux PHANTOM Int) ('Text "Ok") |])
-- ()
--
-- but not in @b@:
--
-- >>> :eval_error $(te[t| UnlessPhantom (Qux Int PHANTOM) ('Text "Bad!") |])
-- ...
-- ... Bad!
-- ...
--
-- Unfortunately there is no known way to emit an error message if the variable
-- /is/ a phantom.
--
-- @since 0.1.0.0
type family UnlessPhantom :: k -> ErrorMessage -> Constraint



------------------------------------------------------------------------------
-- | Like 'UnlessPhantom', but implemented as a first-class family. As such,
-- this allows 'UnlessPhantomFcf' to be chained after 'IfStuck' to only be run
-- if an expression isn't stuck.
--
-- This can be used to avoid emitting false error messages when a type variable
-- isn't phantom, just ambiguous.
--
-- >>> :{
-- data NotPhantomErrorFcf :: k -> Exp Constraint
-- type instance Eval (NotPhantomErrorFcf f) =
--   $(te[t| UnlessPhantom (f PHANTOM) ('Text "It's not phantom!") |])
-- :}
--
-- >>> :{
-- observe_phantom
--     :: UnlessStuck
--          f
--          (NotPhantomErrorFcf f)
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
-- In the next example, we leave @observe_phantom@ unsaturated, and therefore
-- @f@ isn't yet known. Without guarding the 'UnlessPhantomFcf' behind
-- 'UnlessStuck', this would incorrectly produce the message "It's not
-- phantom!"
--
-- >>> observe_phantom
-- ...
-- ... No instance for (Show (f0 p0 -> ()))
-- ...
--
-- @since 0.1.0.0


------------------------------------------------------------------------------
-- | @'Subst' expr a b@ substitutes all instances of @a@ for @b@ in @expr@.
--
-- >>> :kind! $(te[t| Subst (Either Int Int) Int Bool |])
-- ...
-- = Either Bool Bool
--
-- >>> :kind! $(te[t| Subst (Either Int Bool) Int [Char] |])
-- ...
-- = Either [Char] Bool
--
-- >>> :kind! $(te[t| Subst (Either Int Bool) Either (->) |])
-- ...
-- = Int -> Bool
--
-- @since 0.1.0.0
type family Subst :: k1 -> k1 -> k2 -> k2

------------------------------------------------------------------------------
-- | 'VAR' is a meta-varaible which marks a substitution in 'SubstVar'. The
-- result of @'SubstVar' expr val@ is @expr[val/'VAR']@.
--
-- 'VAR' is polykinded and can be used in several settings.
--
-- See 'SubstVar' for examples.
--
-- @since 0.1.0.0
type family VAR :: k


------------------------------------------------------------------------------
-- | Like 'Subst', but uses the explicit meta-variable 'VAR' to mark
-- substitution points.
--
-- >>> :kind! $(te[t| SubstVar (Either VAR VAR) Bool |])
-- ...
-- = Either Bool Bool
--
-- >>> :kind! $(te[t| SubstVar (Either VAR Bool) [Char] |])
-- ...
-- = Either [Char] Bool
--
-- >>> :kind! $(te[t| SubstVar (VAR Int Bool :: Type) (,) |])
-- ...
-- = (Int, Bool)
--
-- @since 0.1.0.0
type family SubstVar :: k1 -> k2 -> k2

