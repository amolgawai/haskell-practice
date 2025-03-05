module Lib
  ( parseArgs,
    numberLines,
  )
where

parseArgs :: [String] -> Maybe FilePath
parseArgs [filePath] = Just filePath
parseArgs _ = Nothing

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

numberLines :: [String] -> NumberedLines
numberLines lines =
  let aux :: Int -> [String] -> NumberedLines
      aux _ [] = []
      aux n (x : xs) = (Just n, x) : aux (n + 1) xs
   in aux 1 lines