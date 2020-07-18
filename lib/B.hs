module B where

{-@ notThree :: {v : Nat | v != 3 } @-}
notThree :: Int
notThree = 3
