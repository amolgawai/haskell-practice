module Lib
    -- ( square
    -- ) where
where

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

indexOf :: Char -> Alphabet -> Int
indexOf aChar [] = undefined
indexOf aChar (x:xs) = if x == aChar then 0 else 1 + indexOf aChar xs

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n aChar = alphabet !! ((indexOf aChar alphabet + n) `mod` length alphabet)

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

digitRot :: Int -> Char -> Char
digitRot = alphabetRot digits

rotChar :: Int -> Char -> Char
rotChar n aChar
    | isLower aChar = lowerRot n aChar
    | isUpper aChar = upperRot n aChar
    | isDigit aChar = digitRot n aChar
    | otherwise = aChar

caesar :: Int -> String -> String
caesar n = map (rotChar n)

rot13 :: String -> String
rot13 = caesar 13

rot5 :: String -> String
rot5 = caesar 5

rot135 :: String -> String
rot135 = map (\aChar -> if isDigit aChar then rotChar 5 aChar else rotChar 13 aChar)