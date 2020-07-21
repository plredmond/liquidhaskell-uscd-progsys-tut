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
infixr 9 ==>

-- | iff (equality?)
(<=>) :: Bool -> Bool -> Bool
{-@ (<=>) :: p:Bool -> q:Bool -> {v:Bool | v <=> (p <=> q)} @-}
False <=> False = True
False <=> True  = False
True  <=> True  = True
True  <=> False = False

-- ** examples: propositions

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

-- | predicates using implication
--
-- "p ==> q" reads "if p is true then q must also be true"

ex3 :: Bool -> Bool -> Bool
{-@ ex3 :: Bool -> Bool -> TRUE @-}
ex3 a b = (a && b) ==> a

ex4 :: Bool -> Bool -> Bool
{-@ ex4 :: Bool -> Bool -> TRUE @-}
ex4 a b = (a && b) ==> b

-- | exercise: Implications and Or
--
-- "shuffle the variables around without changing the operators to make the formula valid"

{-@ ignore ex3' @-}
ex3' :: Bool -> Bool -> Bool
{-@ ex3' :: Bool -> Bool -> TRUE @-}
ex3' a b = (a || b) ==> a

ex3completed :: Bool -> Bool -> Bool
{-@ ex3completed :: Bool -> Bool -> TRUE @-}
ex3completed a b = a ==> (a || b)

-- | the following encode modus ponens
--
-- "if you know that a implies b and you know a is true then it must be the
-- case that b is true"

ex6 :: Bool -> Bool -> Bool
{-@ ex6 :: Bool -> Bool -> TRUE @-}
ex6 a b = (a && (a ==> b)) ==> b

ex7 :: Bool -> Bool -> Bool
{-@ ex7 :: Bool -> Bool -> TRUE @-}
ex7 a b = a ==> ((a ==> b) ==> b)

-- | predicates using iff

exDeMorgan1 :: Bool -> Bool -> Bool
{-@ exDeMorgan1 :: Bool -> Bool -> TRUE @-}
exDeMorgan1 a b = not (a || b) <=> (not a && not b)

-- | exercise: demorgan's law
--
-- "fig the incorrect .."

{-@ ignore exDeMorgan2 @-}
exDeMorgan2 :: Bool -> Bool -> Bool
{-@ exDeMorgan2 :: Bool -> Bool -> TRUE @-}
exDeMorgan2 a b = not (a && b) <=> (not a && not b)

exDeMorgan2completed :: Bool -> Bool -> Bool
{-@ exDeMorgan2completed :: Bool -> Bool -> TRUE @-}
exDeMorgan2completed a b = not (a && b) <=> (not a || not b)

-- ** examples: arithmetic

ax0 :: Bool
{-@ ax0 :: TRUE @-}
ax0 = 1 + 1 == (2 :: Int) -- addition of ones is equal to two

{-@ ignore ax0' @-}
ax0' :: Bool
{-@ ax0' :: TRUE @-}
ax0' = 1 + 1 == (3  :: Int) -- addition of ones is equal to three

ax1 :: Int -> Bool
{-@ ax1 :: Int -> TRUE @-}
ax1 x = x < x + 1 -- add to a number and the result is greater

ax2 :: Int -> Bool
{-@ ax2 :: Int -> TRUE @-}
ax2 x = (x < 0) ==> (0 <= 0 - x) -- subtract a negative number to get a positive number

ax3 :: Int -> Int -> Bool
{-@ ax3 :: Int -> Int -> TRUE @-}
ax3 x y = (0 <= x) ==> (0 <= y) ==> (0 <= x + y) -- the sum of two natural numbers is a natural number

ax4 :: Int -> Int -> Bool
{-@ ax4 :: Int -> Int -> TRUE @-}
ax4 x y = (x == y - 1) ==> (x + 2 == y + 1) -- if y is one greater than x, then two greater than x is one greater than y

ax5 :: Int -> Int -> Int -> Bool
{-@ ax5 :: Int -> Int -> Int -> TRUE @-}
ax5 x y z = (x <= 0 && x >= 0) -- constraint x to be zero
        ==> (y == x + z) -- constrain y to z plus x=zero
        ==> (y == z) -- constraint y to z

-- | exercise: Addition and Order
--
-- "why is the formula not valid? change the hypothesis (left of ==>) to make it valid"

{-@ ignore ax6 @-}
ax6 :: Int -> Int -> Bool
{-@ ax6 :: Int -> Int -> TRUE @-}
ax6 x y = True ==> (x <= x + y) -- it is implied that x is less-equal than x plus y

-- this is invalid because y could be negative

ax6completed :: Int -> Int -> Bool
{-@ ax6completed :: Int -> Int -> TRUE @-}
ax6completed x y = (y >= 0) ==> (x <= x + y)

-- * examples: uninterpreted function

-- z3 only knows that functions return equal outputs on equal inputs
-- "axiom of congruence"

-- | define an uninterpreted function
{-@ measure f :: Int -> Int @-}

-- | "test the axiom of congruence"
congruence     :: (Int -> Int) -> Int -> Int -> Bool
{-@ congruence :: (Int -> Int) -> Int -> Int -> TRUE @-}
congruence f x y = (x == y) ==> (f x == f y)
-- FIXME: this doesn't work
{-@ ignore congruence @-}
