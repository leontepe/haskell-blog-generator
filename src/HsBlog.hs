module HsBlog
  ( convertSingle
  , convertDirectory
  , process
  , buildIndex
  , module HsBlog.Utilities
  )
  where
  
import HsBlog.Utilities

import qualified HsBlog.Markup as Markup
import qualified HsBlog.Html as Html
import HsBlog.Convert (convert)
import HsBlog.Directory (convertDirectory, buildIndex)

import System.IO (hGetContents, hPutStrLn, Handle)

convertSingle :: Html.Title -> Handle -> Handle -> IO ()
convertSingle title input output = do
  content <- hGetContents input
  hPutStrLn output (process title content)

process :: Html.Title -> String -> String
process title = Html.render . convert title . Markup.parse