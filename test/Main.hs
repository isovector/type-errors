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
  , "-XKindSignatures"
  , "-XPolyKinds"
  , "-XScopedTypeVariables"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XUndecidableInstances"
  , "-XTypeApplications"
  , "-XFlexibleContexts"

#if __GLASGOW_HASKELL__ < 806
  , "-XTypeInType"
#endif

  , "src/Lib.hs"
  ]
#endif
