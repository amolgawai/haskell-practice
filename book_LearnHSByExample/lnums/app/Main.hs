module Main (main) where

import System.Environment

printHelpText :: String -> IO ()
printHelpText msg = do
  putStrLn (msg ++ "\n")
  progName <- getProgName
  putStrLn ("Usage: " ++ progName ++ " <filename>")

parseArgs :: [String] -> Maybe FilePath
parseArgs [filePath] = Just filePath
parseArgs _ = Nothing

main :: IO ()
main = do
  cliArgs <- getArgs
  let mFilePath = parseArgs cliArgs
  maybe
    (printHelpText "Missing Filename argument")
    putStrLn
    mFilePath