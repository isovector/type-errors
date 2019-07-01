
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



data AnythingOfAnyKind


type family IfStuck (expr :: k) (b :: k1) (c :: Exp k1) :: k1 where
  IfStuck (_ AnythingOfAnyKind) b c = b
  IfStuck a                     b c = Eval c

type UnlessStuck a c = IfStuck a NoError c
type WhenStuck a b   = IfStuck a b NoErrorFcf

type NoError = (() :: Constraint)
type NoErrorFcf = Pure NoError


data DelayErrorFcf :: ErrorMessage -> Exp k
type instance Eval (DelayErrorFcf a) = TypeError a

type DelayError err = Eval (DelayErrorFcf err)


data Var
type family SubMe :: Type -> k

type VAR = SubMe Var

type family SubstVar (e :: k1) (r :: k2) :: k1 where
  SubstVar (_ Var) r = r
  SubstVar (a b) r   = SubstVar a r (SubstVar b r)
  SubstVar a r       = a

type family Subst (e :: k1) (var :: k2) (sub :: k2) :: k1 where
  Subst var var sub   = sub
  Subst (a b) var sub = Subst a var sub (Subst b var sub)
  Subst a var sub     = a




type PHANTOM = VAR
data UnlessPhantomFcf :: k -> ErrorMessage -> Exp Constraint
type instance Eval (UnlessPhantomFcf exp err) =
  Coercible (SubstVar exp Stuck)
            (SubstVar exp (DelayError err))


type family UnlessPhantom (exp :: k2) (err :: ErrorMessage) where
  UnlessPhantom exp err = Eval (UnlessPhantomFcf exp err)

type family PrettyPrintList (vs :: [k]) :: ErrorMessage where
  PrettyPrintList '[]       = 'Text ""
  PrettyPrintList '[a]      = ShowTypeQuoted a
  PrettyPrintList '[a, b]   = ShowTypeQuoted a ':<>: 'Text ", and " ':<>: ShowTypeQuoted b
  PrettyPrintList (a ': vs) = ShowTypeQuoted a ':<>: 'Text ", " ':<>: PrettyPrintList vs

type family ShowTypeQuoted (t :: k) :: ErrorMessage where
  ShowTypeQuoted (t :: Symbol) = 'ShowType t
  ShowTypeQuoted t             = 'Text "'" ':<>: 'ShowType t ':<>: 'Text "'"

