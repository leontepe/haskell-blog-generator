module HsBlog.Utilities
  ( askUser
  )
  where

yesNoString :: Maybe Bool -> String
yesNoString defaultValue =
  case defaultValue of
    Just True -> " (Y/n)"
    Just False -> " (y/N)"
    Nothing -> " (y/n)"

askUser :: String -> Maybe Bool -> IO Bool
askUser question defaultValue = do
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
            askUser question defaultValue
        
        Nothing -> do
          putStrLn "Invalid response. Type 'y' for yes, 'n' for no."
          askUser question defaultValue