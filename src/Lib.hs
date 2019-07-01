
module Lib
  ( IfStuck
  , WhenStuck
  , DoError
  , Subst
  , VAR
  , SubstVar
  , PHANTOM
  , UnlessPhantom
    -- * Re-exports
  , Exp
  , Eval
  , Pure
  , TypeError
  , ErrorMessage (..)
  ) where

import Fcf
import Data.Kind
import GHC.TypeLits
import Data.Coerce



data AnythingOfAnyKind


type family IfStuck (expr :: k) (b :: k1) (c :: Exp k1) :: k1 where
  IfStuck (_ AnythingOfAnyKind) b c = b
  IfStuck a                     b c = Eval c

type WhenStuck a b = IfStuck a b (Pure (() :: Constraint))


data DoError :: ErrorMessage -> Exp k
type instance Eval (DoError a) = TypeError a


data Var
type family SubMe :: Type -> k

type VAR = SubMe Var

type family SubstVar (e :: k1) (r :: k2) :: k1 where
  SubstVar (_ Var) r = r
  SubstVar (a b) r          = SubstVar a r (SubstVar b r)
  SubstVar a r              = a

type family Subst (e :: k1) (var :: k2) (sub :: k2) :: k1 where
  Subst var var sub   = sub
  Subst (a b) var sub = Subst a var sub (Subst b var sub)
  Subst a var sub     = a




data Phantom p = Phantom

type PHANTOM = VAR
type family UnlessPhantom (exp :: k2) (err :: ErrorMessage) where
  UnlessPhantom exp err =
    Coercible (SubstVar exp Stuck)
              (SubstVar exp (Eval (DoError err)))

type family PrettyPrint (vs :: [k]) :: ErrorMessage where
  PrettyPrint '[]       = 'Text ""
  PrettyPrint '[a]      = ShowTypeQuoted a
  PrettyPrint '[a, b]   = ShowTypeQuoted a ':<>: 'Text ", and " ':<>: ShowTypeQuoted b
  PrettyPrint (a ': vs) = ShowTypeQuoted a ':<>: 'Text ", " ':<>: PrettyPrint vs

type family ShowTypeQuoted (t :: k) :: ErrorMessage where
  ShowTypeQuoted (t :: Symbol) = 'ShowType t
  ShowTypeQuoted t             = 'Text "'" ':<>: 'ShowType t ':<>: 'Text "'"

foo :: UnlessPhantom (f PHANTOM) ('Text "It's not phantom!") => f p -> ()
foo _ = ()

type family DOIT where
  DOIT = TypeError (PrettyPrint '["yo", "hello"])

bar :: Eval (DoError (PrettyPrint '[1, 2])) => ()
bar = ()

