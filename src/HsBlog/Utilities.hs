module HsBlog.Utilities
  ( ask
  )
  where

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