{-# LANGUAGE OverloadedStrings #-}

module Main where

{- Simple example
This example is intended to be as simple as possible, but containing the most significant parts.
The Overloaded Strings language extension is quite handy because it allows you to write text without
using 'fromString' everywhere.
-}

-- import Text.LaTeX
-- import Text.LaTeX.Packages.Inputenc

-- By executing 'execLaTeXT' you run the 'LaTeXT' monad and make a 'LaTeX' value as output.
-- With 'renderFile' you render it to 'Text' and write it in a file.
main :: IO ()
main = return () -- execLaTeXT simple >>= renderFile "simple.tex"



-- example :: LaTeX
-- example = thePreamble <> document theBody

-- thePreamble :: LaTeX
-- thePreamble =
--     documentclass [] article <> usepackage [utf8] inputenc
--  <> usepackage [] amsmath
--  <> title "Using the Texy Class" <> author "Daniel Díaz"

-- theBody :: LaTeX
-- theBody =
--     maketitle
--  <> "Different types pretty-printed using the " <> texttt "Texy" <> " class:"
--  <> itemize theItems


-- -- It's a good idea to separate the preamble of the body.
-- simple :: Monad m => LaTeXT_ m
-- simple = do
--  thePreamble
--  document theBody

-- -- Preamble with some basic info.
-- thePreamble :: Monad m => LaTeXT_ m
-- thePreamble = do
--  documentclass [] article
--  author "Daniel Herrera"
--  title "Layer3com Interview Programming Problems Solutions"

-- -- Body with a section.
-- theBody :: Monad m => LaTeXT_ m
-- theBody = do
--  maketitle
--  section "Insertion into a binary tree"
--  problem1
--  section "IPv4 Permutations"
--  problem2
--  section "Missing number"
--  problem3
--  -- "This is a simple example using the "
--  -- hatex
--  -- " library. "
--  -- 'textbf' turns characters to bold font (as you already may know).
--  -- textbf "Enjoy!"
--  -- " "
--  -- This is how we nest commands.
--  -- textbf (large "Yoohoo!")

-- problem1 :: Monad m => LaTeXT_ m
-- problem1 = do
--   "Some text answering problem 1"

-- problem2 :: Monad m => LaTeXT_ m
-- problem2 = do
--   "Some text answering problem 2"

-- problem3 :: Monad m => LaTeXT_ m
-- problem3 = do
--   "Some text answering problem 3"








-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Text.LaTeX

-- import Text.LaTeX.Packages.Inputenc
-- import Text.LaTeX.Packages.AMSMath

-- import Data.Ratio
-- import Data.Matrix

-- main :: IO ()
-- main = renderFile "texy.tex" example

-- example :: LaTeX
-- example = thePreamble <> document theBody

-- thePreamble :: LaTeX
-- thePreamble =
--     documentclass [] article <> usepackage [utf8] inputenc
--  <> usepackage [] amsmath
--  <> title "Using the Texy Class" <> author "Daniel Díaz"

-- theBody :: LaTeX
-- theBody =
--     maketitle
--  <> "Different types pretty-printed using the " <> texttt "Texy" <> " class:"
--  <> itemize theItems

-- theItems :: LaTeX
-- theItems = 
--     item Nothing <> math (texy (2 :: Int ,3 :: Integer))
--  <> item Nothing <> math (texy [True,False,False])
--  <> item Nothing <> math (texy (1 % 2 :: Rational,2.5 :: Float))
--  <> item Nothing <> equation_ (texy $ fromList 3 3 [1 .. 9 :: Int])
--  <> item Nothing <> math (texy ("This is a String" :: String))

-- {-# LANGUAGE OverloadedStrings #-}

-- module Main where

-- import Text.LaTeX

-- main :: IO ()
-- main = execLaTeXT example >>= renderFile "Fibs.tex"

-- example :: Monad m => LaTeXT_ m
-- example = do
--  documentclass [] article
--  document exampleBody

-- exampleBody :: Monad m => LaTeXT_ m
-- exampleBody = do
--  "This is an example of how "
--  hatex3
--  " works, printing a table of "
--  "the thirteen first elements of the "
--  "Fibonacci sequence."
--  bigskip
--  center $ underline $ textbf "Fibonacci table"
--  center $ tabular Nothing [RightColumn,VerticalLine,LeftColumn] $ do
--    textbf "Fibonacci number" & textbf "Value"
--    lnbk
--    hline
--    foldr (\n l -> do texy n & texy (fib n)
--                      lnbk
--                      l ) (return ()) [0 .. 12]

-- fibs :: [Int]
-- fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- fib :: Int -> Int
-- fib = (fibs!!)
