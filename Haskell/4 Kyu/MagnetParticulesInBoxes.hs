module Codewars.G964.Doubles where

doubles :: Int -> Int -> Double
doubles maxk maxn
  = sum (concat terms)
  where 
    ks = [1..maxk]
    ns = [1..maxn]
    terms = map (\x -> calculateTerms (fromIntegral x) ns) ks

calculateTerms :: Int -> [Int] -> [Double]
calculateTerms k ns
  = [1/(fromIntegral k*((fromIntegral n+1)^(2*k))) | n <- ns ]

{-
Link to the Kata: https://www.codewars.com/kata/56c04261c3fcf33f2d000534
-}