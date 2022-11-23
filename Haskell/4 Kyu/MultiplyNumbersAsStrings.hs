module MultNumAsStrings where

import Data.Char (intToDigit, digitToInt)

-- | mulitply two numbers as strings
multiply :: String -> String -> String
multiply xs ys = show (x * y)
  where
    x = read xs :: Integer
    y = read ys :: Integer

{-

Link to the Kata: https://www.codewars.com/kata/55911ef14065454c75000062/haskell

DESCRIPTION:
This is the first part. You can solve the second part here when you are done with this. Multiply two numbers! Simple!

The arguments are passed as strings.
The numbers may be way very large
Answer should be returned as a string
The returned "number" should not start with zeros e.g. 0123 is invalid
Note: 100 randomly generated tests!

-}