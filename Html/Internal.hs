module Html.Internal where

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
-- h1 content = Structure (el "h1" (escape content))
h1_ = Structure . el "h1" . escape

html_ :: String -> Structure -> Html
html_ title content = Html
  (el "html"
    (el "head"
      ((el "title" . escape) title)
      <>
      (el "body" . getStructureString) content)
  )

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

newtype Html = Html String
newtype Structure = Structure String

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure string -> string

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

