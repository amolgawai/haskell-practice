module Lib
  ( parseArgs,
    numberLines,
    prettyPrintNumberedLines,
    Alignment (AlignLeft, AlignRight, AlignCenter),
    align,
  )
where

parseArgs :: [String] -> Maybe FilePath
parseArgs [filePath] = Just filePath
parseArgs _ = Nothing

type NumberedLine = (Maybe Int, String)

type NumberedLines = [NumberedLine]

numberLines :: [String] -> NumberedLines
numberLines theLines =
  let aux :: Int -> [String] -> NumberedLines
      aux _ [] = []
      aux n (x : xs) = (Just n, x) : aux (n + 1) xs
   in aux 1 theLines

data Alignment = AlignLeft | AlignRight | AlignCenter

align :: Alignment -> Int -> String -> String
align alignment padding strToAlign =
  let diff = padding - length strToAlign
   in case alignment of
        AlignLeft -> strToAlign ++ replicate diff ' '
        AlignRight -> replicate diff ' ' ++ strToAlign
        AlignCenter -> replicate (diff `div` 2) ' ' ++ strToAlign ++ replicate (diff `div` 2) ' '

prettyPrintNumberedLines :: Alignment -> NumberedLines -> [String]
prettyPrintNumberedLines alignment numberedLines =
  let (lineNums, textLines) = unzip numberedLines
      numberStrs = map (maybe "" show) lineNums
      maxLineNumberLength = maximum $ map length numberStrs
      alignedNumStrs = map (align alignment maxLineNumberLength) numberStrs
   in zipWith (\numStr txt -> numStr ++ " " ++ txt) alignedNumStrs textLines