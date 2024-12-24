module Lib
    -- ( square
    -- ) where
where
import Data.Char (isDigit)

square :: Int -> Int
square x = x * x

type Alphabet = [Char]
lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']
upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']
digits :: Alphabet
digits = ['0' .. '9']

isLower :: Char -> Bool
isLower aChar = aChar `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper aChar = aChar `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit aChar = aChar `elem` digits

isMisc :: Char -> Bool
isMisc aChar = aChar `notElem` lowerAlphabet ++ upperAlphabet ++ digits