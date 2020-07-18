module Sec2 where

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

{-@ ignore ex0' @-}
ex0' :: Bool
{-@ ex0' :: TRUE @-}
ex0' = False
