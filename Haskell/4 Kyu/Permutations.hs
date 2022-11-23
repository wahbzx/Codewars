module Codewars.Kata.Permutations (permutations) where
import Data.List hiding (permutations)

rotate :: String -> [String]
rotate xs = take (length xs) (iterate (\(y:ys) -> ys ++ [y]) xs)

permutations :: String -> [String]
permutations []     = [[]]
permutations (x:xs) = nub $ concatMap (rotate.(x:)) (permutations xs)

{-
Link to the kata: https://www.codewars.com/kata/5254ca2719453dcc0b00027d

DESCRIPTION:
In this kata you have to create all permutations of a non empty input string and remove duplicates, 
if present. This means, you have to shuffle all letters from the input in all possible orders.

Examples:

* With input 'a'
* Your function should return: ['a']
* With input 'ab'
* Your function should return ['ab', 'ba']
* With input 'aabb'
* Your function should return ['aabb', 'abab', 'abba', 'baab', 'baba', 'bbaa']
The order of the permutations doesn't matter.

-}