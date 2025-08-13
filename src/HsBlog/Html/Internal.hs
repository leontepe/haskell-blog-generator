module HsBlog.Html.Internal where

import Numeric.Natural

-- types

newtype Html =
  Html String

newtype Structure =
  Structure String

newtype Content =
  Content String

type Title = String

-- edsl

html_ :: Title -> Structure -> Html
html_ title content = Html
  (el "html"
    (el "head"
      ((el "title" . escape) title)
      <>
      (el "body" . getStructureString) content)
  )

-- structure

p_ :: Content -> Structure
p_ = Structure . el "p" . getContentString

h_ :: Natural -> Content -> Structure
h_ n = Structure . el ("h" <> show n) . getContentString

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

instance Monoid Structure where
  mempty = Structure ""

-- content

txt_ :: String -> Content
txt_ = Content . escape

link_ :: FilePath -> Content -> Content
link_ path content = 
  Content $
    elAttr
      "a"
      ("href=\"" <> escape path <> "\"")
      (getContentString content)

img_ :: FilePath -> Content
img_ path =
  Content $ "<img src=\"" <> escape path <> "\">"

b_ :: Content -> Content
b_ content =
  Content $ el "b" (getContentString content)

i_ :: Content -> Content
i_ content =
  Content $ el "i" (getContentString content)

instance Semigroup Content where
  (<>) (Content string1) (Content string2) =
    Content $ string1 <> string2

instance Monoid Content where
  mempty = Content ""

-- render

render :: Html -> String
render (Html html) = html

-- utilities

el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

elAttr :: String -> String -> String -> String
elAttr tag attrs content =
  "<" <> tag <> " " <> attrs <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure string) = string

getContentString :: Content -> String
getContentString (Content string) = string

escape :: String -> String
escape =
  let
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&#39;"
        _ -> [c]
  in
    concatMap escapeChar
