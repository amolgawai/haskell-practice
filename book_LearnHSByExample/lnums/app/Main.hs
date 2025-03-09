module Main (main) where

import Lib
import System.Environment

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

readLines :: FilePath -> IO [String]
readLines filePath = do
  contents <- readFile filePath
  return (lines contents)

printFile :: FilePath -> IO ()
printFile filePath = do
  mContent <- readLines filePath
  let mNumberedLines = numberLines mContent
  let prettyLines = prettyPrintNumberedLines AlignRight mNumberedLines
  mapM_ putStrLn prettyLines

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArgs cliArgs
  maybe
    (printHelpText "Missing Filename argument")
    printFile
    mFilePath