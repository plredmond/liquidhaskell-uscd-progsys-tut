module Sec2 where

-- https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
--import Paths_ucsd_progsys_lh_tut (version)

-- $setup
-- >>> import Test.QuickCheck

-- * Section 2
--
-- Tutorial from here: <https://ucsd-progsys.github.io/liquidhaskell-tutorial/02-logic.html>

-- ** semantics

-- | implication
(==>) :: Bool -> Bool -> Bool
{-@ (==>) :: p:Bool -> q:Bool -> {v:Bool | v <=> (p ==> q)} @-}
False ==> False = True
False ==> True  = True
True  ==> True  = True
True  ==> False = False

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

-- | "predicate that states that a bool is either True or False"
ex1 :: Bool -> Bool
{-@ ex1 :: Bool -> TRUE @-}
ex1 b = b || not b

-- | predicate that states that "a variable cannot be both True and False"
ex2 :: Bool -> Bool
{-@ ex2 :: Bool -> FALSE @-}
ex2 b = b && not b

-- ** exercise: implications and or

-- CONTINUE: this exercise

-- | predicates using implication
--
-- "p ==> q" reads "if p is true then q must also be true"

ex3 :: Bool -> Bool -> Bool
{-@ ex3 :: Bool -> Bool -> TRUE @-}
ex3 a b = (a && b) ==> a

-- CONTINUE: ex4
