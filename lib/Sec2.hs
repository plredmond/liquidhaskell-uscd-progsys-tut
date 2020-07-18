{-# LANGUAGE NoImplicitPrelude, PackageImports #-}
{-@ LIQUID "--no-termination" @-} -- turn off termination checking, which is important for some examples
module Sec2 where

import "liquid-base" Prelude

-- https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
--import Paths_ucsd_progsys_lh_tut (version)

-- $setup
-- >>> import Test.QuickCheck

-- * Section 2
--
-- Tutorial from here: <https://ucsd-progsys.github.io/liquidhaskell-tutorial/02-logic.html>

-- | notes
--
--  * predicate is satisfiable if there exists an assignment that makes the
--  predicate evaluate to true
--
--  * predicate is valid if every assignment in that environment makes the
--  predicate evaluate to true

{-@ type TRUE = {v:Bool | v} @-}
{-@ type FALSE = {v:Bool | not v} @-}

ex0 :: Bool
{-@ ex0 :: TRUE @-}
ex0 = True

ex0' :: Bool
{-@ ex0' :: TRUE @-}
ex0' = False

{-@ notThree :: {v : Nat | v != 3 } @-}
notThree :: Int
notThree = 3
