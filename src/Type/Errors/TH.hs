{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Type.Errors.TH where

import Control.Applicative
import Language.Haskell.TH
import Data.Generics


type family Subst :: k1 -> k1 -> k2 -> k2
type family SubstVar :: k1 -> k2 -> k2
type family VAR :: k


flipEverywhere :: Data a => a -> (forall a1. Data a1 => a1 -> a1) -> a
flipEverywhere a f = everywhere f a


replaceWhen :: Data a => Type -> Type -> a -> a
replaceWhen a b = everywhere $ mkT $ \case
  x | x == a -> b
  x -> x


parseSubst :: Type -> Type
parseSubst (ConT subst `AppT` t `AppT` a `AppT` b)
  | subst == ''Subst = replaceWhen a b t
parseSubst (ConT subst `AppT` t `AppT` b)
  | subst == ''SubstVar = replaceWhen (ConT ''VAR) b t
parseSubst a = a


tt :: Q Type -> Q Type
tt = liftA parseSubst

