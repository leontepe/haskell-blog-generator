{-# LANGUAGE LambdaCase #-}

module Convert where

import qualified Markup
import qualified Html

import System.Directory (doesFileExist)
import System.Environment (getArgs)
import Control.Monad (when)

convert :: Html.Title -> Markup.Document -> Html.Html
-- don't quite understand what foldMap is doing here
convert title = Html.html_ title . foldMap convertStructure

convertStructure :: Markup.Structure -> Html.Structure
convertStructure structure =
  case structure of
    Markup.Heading n txt ->
      Html.h_ n txt

    Markup.Paragraph p ->
      Html.p_ p

    Markup.UnorderedList list ->
      Html.ul_ $ map Html.p_ list

    Markup.OrderedList list ->
      Html.ol_ $ map Html.p_ list

    Markup.CodeBlock list ->
      Html.code_ (unlines list)

-- getArgs :: IO [String] -- Get the program arguments

-- getContents :: IO String -- Read all of the content from stdin

-- readFile :: FilePath -> IO String -- Read all of the content from a file

-- writeFile :: FilePath -> String -> IO () -- Write a string into a file

-- doesFileExist :: FilePath -> IO Bool -- Checks whether a file exists

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse

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

whenIO :: IO Bool -> IO () -> IO ()
whenIO conditionTask action = do
  condition <- conditionTask
  when condition action

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
