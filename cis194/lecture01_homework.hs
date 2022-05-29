{- stack script
 --resolver ghc-9.0.2
-}

module Leture01_homework where

main :: IO ()
main = do
  putStrLn "lecture01 homework"
  let ex1 = toDigits 1234 == [1, 2, 3, 4]
  putStrLn ("toDigits 1234 == [1, 2, 3, 4]: " ++ show ex1)
  let ex2 = toDigitsRev 1234 == [4, 3, 2, 1]
  putStrLn ("toDigitsRev 1234 == [4, 3, 2, 1]: " ++ show ex2)
  let ex3 = null (toDigits 0)
  putStrLn ("toDigits 0 == []: " ++ show ex3)
  let ex4 = null (toDigits (-17))
  putStrLn ("toDigits (-17) == []: " ++ show ex4)
  let ex5 = doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]
  putStrLn ("doubleEveryOther [8, 7, 6, 5] == [16, 7, 12, 5]: " ++ show ex5)
  let ex6 = doubleEveryOther [1, 2, 3] == [1, 4, 3]
  putStrLn ("doubleEveryOther [1, 2, 3] == [1, 4, 3]: " ++ show ex6)
  let ex7 = sumDigits [16, 7, 12, 5] == 1 + 6 + 7 + 1 + 2 + 5
  putStrLn ("sumDigits [16, 7, 12, 5] == 1 + 6 + 7 + 1 + 2 + 5 == 22: " ++ show ex7)
  let ex8 = validate 4012888888881881
  putStrLn ("validate 4012888888881881 == True: " ++ show ex8)
  let ex9 = not (validate 4012888888881882)
  putStrLn ("validate 4012888888881882 == False: " ++ show ex9)

-- Homework 1
-- Validating Credit Card Numbers
-- Rules:
-- 1.Double the value of every second digit beginning from the right.
--   That is, the last digit is unchanged; the second-to-last digit is
--   doubled; the third-to-last digit is unchanged; and so on.
--   For example, [1, 3, 8, 6] becomes [2, 3, 16, 6]
-- 2.Add the digits of the doubled values and the undoubled digits from
--   the original number. For example, [2, 3, 16, 6]
--   becomes 2+3+1+6+6 = 18.
-- 3.Calculate the remainder when the sum is divied by 10. For the above
--   example, the remainder would be 8.
--   If the result equals to 0, then the number is valid.

-- Exercise 1 We need to first find the digits of a number.
-- toDigits should convert positive Integers to a list of digits.
-- For 0 or negative inputs, should return the empty list.
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- toDigitsRev should do the same, but with the digits reversed.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = revList (toDigits n)

revList :: [Integer] -> [Integer]
revList [] = []
revList (x : ys) = revList ys ++ [x]

-- Exercise 2 Once we have the digits in the proper order, we need to
-- double every other one.
-- doubleEveryOther should double every other number beginning from
-- the right, that is , the second-to-last, fourth-to-last, ...numbers
-- are doubled.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = revList (doubleEveryOtherRev (revList x))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x : y : zs) = x : (2 * y : doubleEveryOtherRev zs)

-- Exercise 3 The output of doubleEveryOther has a mix of one-digit
-- and two-digit numbers.
-- sumDigits should calculate the sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x : ys) =
  if x < 10
    then x + sumDigits ys
    else sumDigits (toDigits x) + sumDigits ys

-- Exercise 4
-- validate should indicates whether an Integer could be a valid credi card number.
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n)) `mod` 10) == 0

-- The Towers of Hanoi
-- Exercise 5
-- Rules:
-- 1.you may only move one disk at a time, and
-- 2.a larger disk may never be stacked on top of a smaller one.
-- given the number of discs and names of three pegs, hanoi
-- should return a list of moves to be performed to move the stack
-- of discs from the first peg to the second.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
-- TODO: implement rules
hanoi n a b c = []

type Peg = String

type Move = (Peg, Peg)
