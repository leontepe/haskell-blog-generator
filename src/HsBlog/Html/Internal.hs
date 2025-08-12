module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html = Html String
newtype Structure = Structure String
type Title = String

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

html_ :: Title -> Structure -> Html
html_ title content = Html
  (el "html"
    (el "head"
      ((el "title" . escape) title)
      <>
      (el "body" . getStructureString) content)
  )

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

getStructureString :: Structure -> String
getStructureString (Structure string) = string

render :: Html -> String
render (Html html) = html

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

instance Semigroup Structure where
  (<>) (Structure a) (Structure b) = Structure (a <> b)

empty_ :: Structure
empty_ = Structure ""

instance Monoid Structure where
  mempty = empty_