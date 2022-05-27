{- stack script
 --resolver ghc-9.0.2
-}

module Lecture01 where

main :: IO ()
main = do
  putStrLn ("(sum (map (3 *) [1 .. 10])) = " ++ show (sum (map (3 *) [1 .. 10])))
  putStrLn ("biggestInt = " ++ show biggestInt ++ "\nsmallestInt = " ++ show smallestInt)
  putStrLn ("boolean true = " ++ show t ++ "\nunicode char c = " ++ show c ++ "\nstring s = " ++ s)
  putStrLn
    ( "arithmetic:\n3 + 2 = " ++ show (3 + 2)
        ++ "\n19 - 27 = "
        ++ show (19 - 27)
        ++ "\n2.35 - 8.6 = "
        ++ show (2.35 * 8.6)
        ++ "\ninteger division: 19 `div` 3 = "
        ++ show (19 `div` 3)
        ++ "\nmod 19 3 = "
        ++ show (mod 19 3)
        ++ "\n19 `mod` 3 = "
        ++ show (19 `mod` 3)
        ++ "\n7 ^ 222 = "
        ++ show (7 ^ 222)
        ++ "\n(-3) * (-7) = "
        ++ show ((-3) * (-7))
    )
  putStrLn "define functions:"
  putStrLn ("hailstone 10 = " ++ show (hailStone 10))
  putStrLn "function with multiple arguments:"
  putStrLn ("fib 100 = " ++ show (fib 100))
  putStrLn
    ( "lists:\n[Integer] [2, 4 .. 100] = " ++ show intLst
        ++ "\n[Char] = "
        ++ show charLst
    )
  putStrLn ("constructing lists: (haileStoneSeq 10) = " ++ show (hailStoneSeq 10))
  putStrLn
    ( "functions on lists: (sumEveryTwo [3, 5, 6, 13, 7, 5]) = "
        ++ show (sumEveryTwo [3, 5, 6, 13, 7, 5])
    )
  putStrLn
    ( "length of (hailStoneSeq 10) with lazy evaluation uses only O(1) memory: "
        ++ show (hailStoneLen 10)
    )

-- basic types
biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound

t, f :: Bool
t = True
f = False

c :: Char
c = 'Ø'

s :: String
s = "Hello Haskel!"

-- normal (non-literate) comments
{- multiline
  comments
-}

-- function
hailStone :: Integer -> Integer
hailStone n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

fib :: Integer -> Integer
fib n = fibIter n 0 1

-- tail recursion
fibIter :: Integer -> Integer -> Integer -> Integer
fibIter x y z
  | x == 0 = y
  | otherwise = fibIter (x -1) z (y + z)

-- lists
intLst :: [Integer]
intLst = [2, 4 .. 100]

charLst :: [Char]
charLst = ['h', 'e', 'l', 'l', 'o']

-- constructing lists
-- [2, 3, 4] == 2 : 3 : 4 : []
-- generate the sequence of hailstone iterations from a starting number
hailStoneSeq :: Integer -> [Integer]
hailStoneSeq 1 = [1]
hailStoneSeq n = n : hailStoneSeq (hailStone n)

-- functions on lists
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo [x] = [x]
sumEveryTwo (x : (y : zs)) = (x + y) : sumEveryTwo zs

-- Compute the length of a list of Integers.
intListLength :: [Integer] -> Integer
intListLength = foldr (\x -> (+) 1) 0

-- combine functions
hailStoneLen :: Integer -> Integer
hailStoneLen n = intListLength (hailStoneSeq n) - 1
