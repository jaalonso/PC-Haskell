module Factorial
    ( factorial
    ) where

-- | Factorial
--
-- >>> factorial 3
-- 6
factorial :: Int -> Int
factorial n = product [1..n]
