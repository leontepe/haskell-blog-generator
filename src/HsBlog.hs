{-# LANGUAGE LambdaCase #-}

module HsBlog
  ( main
  , process
  )
  where

import HsBlog.Convert (process)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

yesNoString :: Maybe Bool -> String
yesNoString defaultValue =
  case defaultValue of
    Just True -> " (Y/n)"
    Just False -> " (y/N)"
    Nothing -> " (y/n)"

ask :: String -> Maybe Bool -> IO Bool
ask question defaultValue = do
  putStrLn (question <> yesNoString defaultValue)
  answer <- getLine
  case answer of
    "y" -> pure True
    "n" -> pure False
    _ ->
      case defaultValue of
        Just bool -> case answer of
          -- empty string is Enter
          "" -> pure bool
          _ -> do
            putStrLn "Invalid response. Type 'y' for yes, 'n' for no, or Enter to accept the default."
            ask question defaultValue
        
        Nothing -> do
          putStrLn "Invalid response. Type 'y' for yes, 'n' for no."
          ask question defaultValue

main :: IO ()
main = getArgs >>= \case
  -- read from stdin, write to stdout
  [] -> do
    stdin <- getContents
    putStrLn (process "Empty title" stdin)

  -- input/output filenames
  [inputFileName, outputFileName] -> do
    content <- readFile inputFileName
    outputExists <- doesFileExist outputFileName
    let processThenWrite = writeFile outputFileName (process "Empty title" content)
    if outputExists
      then do
        overwriteOutput <- ask "The output file already exists. Do you want to overwrite it?" (Just False)
        if overwriteOutput
          then processThenWrite
          else putStrLn "Aborted."
      else processThenWrite

  _ -> putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"
