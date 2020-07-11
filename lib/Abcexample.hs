module Abcexample where

-- https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
import qualified Paths_abcexample

-- $setup
-- >>> import Test.QuickCheck


-- | Main in the library
--
-- >>> 1 + 1
-- 2
--
-- prop> \(Small n) -> n * 2 == n + n
main :: IO ()
main = do
    putStrLn "Hello world!"
    putStrLn $ "abcexample is " <> show Paths_abcexample.version

{-@ one :: {v:Int | v = 1 } @-}
one :: Int
one = 1

{-@ assume notThree :: {v : Nat | v != 3 } @-}
notThree :: Int
notThree = 4

{-@ two :: Nat @-}
two :: Int
two = one + one
