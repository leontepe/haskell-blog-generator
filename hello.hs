-- lamdba: \a b -> a + b
-- `f . g x` = f (g x)

import Html

main :: IO ()
main = putStrLn (render myhtml)

myhtml =
  html_
    "my <html> page"
    ( h1_ "hello, world"
        <> p_ "my <p> paragraph 1"
        <> p_ "paragraph 2"
    )