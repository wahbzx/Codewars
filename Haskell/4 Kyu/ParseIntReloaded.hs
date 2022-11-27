module ParseIntReloaded where
import Data.Maybe (fromJust)
import Data.List ( groupBy )

--check prefixes of words for 1-99
--for 100 and 1000 multiply the number before it by those
prefixes =
  [("zero", 0),
  ("one",1),
  ("two",2),
  ("three",3),
  ("four",4),
  ("five",5),
  ("six",6),
  ("seven",7),
  ("eight",8),
  ("nine", 9),
  ("ten", 10),
  ("eleven",11),
  ("twelve",12),
  ("thirteen",13),
  ("fourteen",14),
  ("fifteen",15),
  ("sixteen",16),
  ("seventeen",17),
  ("eighteen",18),
  ("nineteen",19),
  ("twenty", 20),
  ("thirty", 30),
  ("forty", 40),
  ("fifty", 50),
  ("sixty", 60),
  ("seventy", 70),
  ("eighty", 80),
  ("ninety", 90),
  ("hundred", 100),
  ("thousand", 1000),
  ("million", 1000000)]

a =[[7,100,80,3],[1000],[9,100,19]]

parseInt :: String -> Int
parseInt s
  = fun $ map fun s2
  where
    s2 = genList s
    fun = foldr1 (\x -> if checkten x then (x*) else (+x)) . reverse


genList :: String -> [[Int]]
genList s
  =  groupBy (\x y-> (x+y) < 1000) list
  where
    s2 = map (\x -> if x=='-' then ' ' else x) s
    list = map (\x -> fromJust (lookup x prefixes)) $ filter(/="and") $ words s2

checkten :: Int -> Bool
checkten n
  = sum (filter (/=0) x) == 1 && (mod n 100 == 0) 
  where
    x = map ((\x -> read x::Int) . (:[])) . show $ n

{-
Link to the Kata: https://www.codewars.com/kata/525c7c5ab6aecef16e0001a5

DESCRIPTION:
In this kata we want to convert a string into an integer. The strings simply represent the numbers in words.

Examples:

"one" => 1
"twenty" => 20
"two hundred forty-six" => 246
"seven hundred eighty-three thousand nine hundred and nineteen" => 783919
Additional Notes:

The minimum number is "zero" (inclusively)
The maximum number, which must be supported is 1 million (inclusively)
The "and" in e.g. "one hundred and twenty-four" is optional, in some cases it's present and in others it's not
All tested numbers are valid, you don't need to validate them
-}