module Sudoku where

import Data.List
import Data.List.Split
import Data.Maybe


sudoku :: [[Int]] -> [[Int]]
sudoku puzzle
  | newPuzzle == puzzle = puzzle
  | otherwise = sudoku newPuzzle
  where
    positions = map (elemIndex True) (findSingletons puzzle)
    replaces = map (fromMaybe (-1)) positions
    values = map (filter ((==1) . length)) (findSolvableValues puzzle)
    newPuzzle = map (\x -> replace (replaces !! x) (head. concat $ values !! x) (puzzle !! x)) [0..8]

less :: (Int, Int) -> (Int, Int) ->  Bool
less (x,y) (x',y')
  | x <= x' && y <= y' = True
  | x > x' = False
  | y > y' = False

puzzle :: [[Int]]
puzzle = [[5,3,0,0,7,0,0,0,0],
  [6,0,0,1,9,5,0,0,0],
  [0,9,8,0,0,0,0,6,0],
  [8,0,0,0,6,0,0,0,3],
  [4,0,0,8,0,3,0,0,1],
  [7,0,0,0,2,0,0,0,6],
  [0,6,0,0,0,0,2,8,0],
  [0,0,0,4,1,9,0,0,5],
  [0,0,0,0,8,0,0,7,9]]


findSquares :: [[Int]] -> [[Int]]
findSquares puzzle
  = map concat a
  where
    a = concatMap (chunksOf 3) . transpose $ map (chunksOf 3) puzzle

findPos :: (Int, Int) -> [[Int]] -> Int
findPos (x, y) puzzle
  = puzzle !! x !! y


checkPos :: Int -> Int -> [[Int]] -> [Int]
checkPos x y puzzle
  | findPos (x,y) puzzle == 0 = [1..9] \\ (([1..9] \\ checkRow x) ++ ([1..9] \\ checkColumn y) ++ ([1..9] \\ checkSquare sqrpos))
  | otherwise = []
  where
    sqrpos = whichSquare (x,y)
    checkRow n = [1..9] \\ puzzle !! n
    checkSquare n = [1..9] \\ findSquares puzzle !! n
    checkColumn n = [1..9] \\ transpose puzzle !! n


whichSquare :: (Int, Int) -> Int
whichSquare pos
  | pos `less` (2,2) = 0
  | pos `less` (2,5) = 3
  | pos `less` (2,8) = 6
  | pos `less` (5,2) = 1
  | pos `less` (5,5) = 4
  | pos `less` (5,8) = 7
  | pos `less` (8,2) = 2
  | pos `less` (8,5) = 5
  | pos `less` (8,8) = 8

findSolvableValues :: [[Int]] -> [[[Int]]]
findSolvableValues puzzle = map (\x -> zipWith (\x y-> checkPos x y puzzle)(replicate 9 x) [0..8]) [0..8]
findSingletons :: [[Int]] -> [[Bool]]
findSingletons puzzle = chunksOf 9 [(==1). length $ (findSolvableValues puzzle !! x !! y) | x <-[0..8], y<-[0..8]]

replace :: Int -> Int -> [Int] ->[Int]
replace (-1) _ list = list
replace pos n list
  =  h1 ++ [n] ++ tail h2
  where
    (h1, h2) = splitAt pos list


{-
Link to the Kata: https://www.codewars.com/kata/5296bc77afba8baa690002d7

DESCRIPTION:
Write a function that will solve a 9x9 Sudoku puzzle. The function will take one argument consisting of the 2D puzzle array, with the value 0 representing an unknown square.

The Sudokus tested against your function will be "easy" (i.e. determinable; there will be no need to assume and test possibilities on unknowns) and can be solved with a brute-force approach.

For Sudoku rules, see the Wikipedia article.

puzzle = [[5,3,0,0,7,0,0,0,0],
          [6,0,0,1,9,5,0,0,0],
          [0,9,8,0,0,0,0,6,0],
          [8,0,0,0,6,0,0,0,3],
          [4,0,0,8,0,3,0,0,1],
          [7,0,0,0,2,0,0,0,6],
          [0,6,0,0,0,0,2,8,0],
          [0,0,0,4,1,9,0,0,5],
          [0,0,0,0,8,0,0,7,9]]

sudoku(puzzle)
# Should return
 [[5,3,4,6,7,8,9,1,2],
  [6,7,2,1,9,5,3,4,8],
  [1,9,8,3,4,2,5,6,7],
  [8,5,9,7,6,1,4,2,3],
  [4,2,6,8,5,3,7,9,1],
  [7,1,3,9,2,4,8,5,6],
  [9,6,1,5,3,7,2,8,4],
  [2,8,7,4,1,9,6,3,5],
  [3,4,5,2,8,6,1,7,9]]


-}