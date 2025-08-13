module Main where

import OptParse
import qualified HsBlog

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO

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
main = do
  options <- parse
  case options of
    ConvertDir input output ->
      HsBlog.convertDirectory input output

    ConvertSingle input output replace -> do
      (title, inputHandle) <-
        case input of
          Stdin ->
            pure ("", stdin)
          InputFile file ->
            (,) file <$> openFile file ReadMode

      outputHandle <-
        case output of
          Stdout -> pure stdout
          OutputFile file -> do
            exists <- doesFileExist file
            shouldOpenFile <-
              if exists && not replace
                then ask "The output file already exists. Do you want to overwrite it?" (Just False)
                else pure True
            if shouldOpenFile
              then
                openFile file WriteMode
              else
                do
                  putStrLn "Aborted."
                  exitFailure

      HsBlog.convertSingle title inputHandle outputHandle
      hClose inputHandle
      hClose outputHandle