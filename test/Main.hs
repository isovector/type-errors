{-# LANGUAGE CPP #-}

module Main where

import Test.DocTest


main :: IO ()
#if __GLASGOW_HASKELL__ < 804
main = pure ()
#else
main = doctest
  [ "-isrc/"
  , "--fast"
  , "-package first-class-families"
  , "-XConstraintKinds"
  , "-XDataKinds"
  , "-XFlexibleContexts"
  , "-XKindSignatures"
  , "-XLambdaCase"
  , "-XPolyKinds"
  , "-XScopedTypeVariables"
  , "-XTemplateHaskell"
  , "-XTypeApplications"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XUndecidableInstances"

#if __GLASGOW_HASKELL__ < 806
  , "-XTypeInType"
#endif

  , "src/Type/Errors.hs"
  ]
#endif
