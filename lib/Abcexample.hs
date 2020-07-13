module Abcexample where

-- https://www.haskell.org/cabal/users-guide/developing-packages.html#accessing-data-files-from-package-code
import qualified Paths_abcexample

main :: IO ()
main = do
    putStrLn "Hello world!"
    putStrLn $ "abcexample is " <> show Paths_abcexample.version

{-@ notThree :: {v : Nat | v != 3 } @-}
notThree :: Int
notThree = 4
