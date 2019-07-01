{-# LANGUAGE CPP #-}

module Main where

import Test.DocTest


main :: IO ()
main = doctest
  [ "-isrc/"
  , "--fast"
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
  , "test/TypeErrors.hs"
  ]

