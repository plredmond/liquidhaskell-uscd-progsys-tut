{-# LANGUAGE NoImplicitPrelude, PackageImports #-}
{-@ LIQUID "--no-termination" @-} -- turn off termination checking, which is important for some examples
module Sec1 where

import "liquid-base" Prelude

-- https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
--import Paths_ucsd_progsys_lh_tut (version)

-- $setup
-- >>> import Test.QuickCheck

-- * Section 1
--
-- Tutorial from here: <https://ucsd-progsys.github.io/liquidhaskell-tutorial/01-intro.html>

average :: [Int] -> Int
average xs = sum xs `div` length xs

-- | examples for average
--
-- >>> average [10, 20, 30, 40]
-- 25
--
-- >>> average []
-- *** Exception: divide by zero

-- | examples for maps
--
-- >>> import Data.Map
-- >>> let m = fromList [ ("haskell", "lazy"), ("ocaml", "eager") ]
-- >>> m ! "haskell"
-- "lazy"
--
-- >>> m ! "javascript"
-- "*** Exception: Map.!: given key is not an element in the map
-- ...

-- | examples for vectors
--
-- >>> import Data.Vector
-- >>> let v = fromList ["haskell", "ocaml"]
-- >>> unsafeIndex v 0
-- "haskell"
--
-- >>> -- unsafeIndex v 3 -- segfault and doctest cannot catch it
-- 

-- | examples for text
--
-- >>> import Data.Text
-- >>> import Data.Text.Unsafe
-- >>> let t = pack "Voltage"
-- >>> takeWord16 5 t
-- "Volta"
--
-- >>> takeWord16 20 t -- displays a security vuln
-- "Voltage\NUL..."

-- | audience section
--
-- * nand vs xor
--
-- a | b | and a b | not.and a b | or a b | xor a b
-- --+---+---------+-------------+--------+--------
-- T | T |   T     |  F          | T      | F
-- F | T |   F     |  T          | T      | T
-- T | F |   F     |  T          | T      | T
-- F | F |   F     |  T          | F      | F
--
-- * forall a. a -> a
--
-- it's universal quantification of a type variable, a, used in a function type
-- a -> a, which can only be inhabited by the id function
