{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Unit1 where

{-# HLINT ignore "Use max" #-}
-- example and exercises from the Unit 1 chapters of the book
-- lesson 2

-- | introducing "where"
--
-- Examples:
--
--  >>> calChange 5 10
--  5
--
--  >>> calChange 10 5
--  0
--
--  >>> calChange 5 10 == calChangeWth 5 10 && calChange 10 5 == calChangeWth 10 5
--  True
calChange :: Int -> Int -> Int
calChange owed given =
  if given - owed > 0
    then given - owed
    else 0

calChangeWth :: Int -> Int -> Int
calChangeWth owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

-- lesson 3 - lambda functions

-- | sum of squares or squares of sum, whichever is greater
--
-- Examples:
-- >>> sumSquareOrSquareSum 1 1
-- 4
--
-- >>> sumSquareOrSquareSum 1 1 == sumSquareOrSquareSumPln  1 1
-- True
--
-- >>> sumSquareOrSquareSum 10 10 == sumSquareOrSquareSumLambda  10 10
-- True
--
-- >>> sumSquareOrSquareSum 1 2 == sumSquareOrSquareSumLet  1 2
-- True
sumSquareOrSquareSum :: Int -> Int -> Int
sumSquareOrSquareSum x y = if sumSquare > squareSum then sumSquare else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

sumSquareOrSquareSumPln :: Int -> Int -> Int
sumSquareOrSquareSumPln x y = if (x ^ 2 + y ^ 2) > (x + y) ^ 2 then x ^ 2 + y ^ 2 else (x + y) ^ 2

sumSquareOrSquareSumLambda :: Int -> Int -> Int
sumSquareOrSquareSumLambda x y =
  ( \sumSquare squareSum ->
      if sumSquare > squareSum then sumSquare else squareSum
  )
    (x ^ 2 + y ^ 2)
    ((x + y) ^ 2)

sumSquareOrSquareSumLet :: Int -> Int -> Int
sumSquareOrSquareSumLet x y =
  let sumSquare = (x ^ 2 + y ^ 2)
      squareSum = (x + y) ^ 2
   in if sumSquare > squareSum then sumSquare else squareSum