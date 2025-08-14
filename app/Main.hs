module Main where

import HsBlog.Utilities (askUser)

import OptParse
import qualified HsBlog

import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import System.IO

main :: IO ()
main = do
  options <- parse
  case options of
    ConvertDir input output env ->
      HsBlog.convertDirectory env input output

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
                then askUser "The output file already exists. Do you want to overwrite it?" (Just False)
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