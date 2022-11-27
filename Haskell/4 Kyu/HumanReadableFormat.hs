module FormatDuration where
import Data.List

-- Pads the given list with the correct amount of 0s
padder :: [Int] -> Int -> [Int]
padder n x
  | length list < x = replicate (x - length list) 0 ++ list
  | otherwise       = list
  where
    list = zipWith (+) (replicate x 0) n

-- Generates a list in the form [y,d,h,m,s]
genTimes :: Int -> [Int]
genTimes n
  | n >= (86400 * 365) = y : padder (genTimes ys) 4
  | n >= 86400         = d : padder (genTimes s) 3
  | q == 0             = [r]
  | r == 0 && q < 60   = q : [0]
  | otherwise          = genTimes q ++ [r]
  where
    (q, r)  = quotRem n 60
    (d, s)  = quotRem n 86400
    (y, ys) = quotRem n (86400 * 365)

-- Formats the generated List according to the spec
formatDuration :: (Integral i) => i -> String
formatDuration x
  | x == 0                = "now"
  | null. tail $ filtered = head filtered
  | otherwise             = finalString
  where
    n = replicate (5- length (genTimes $ fromIntegral x)) 0 ++ genTimes (fromIntegral x)
    strings = zipWith (\x y ->if y==1 then show y++" "++init  x else show y++" "++x) suffixes n
    filtered = filter (\x -> head x /= '0') strings
    suffixes = ["years", "days", "hours", "minutes", "seconds"]
    finalString = concat $ (intersperse ", ". init $ filtered ) ++ [" and "] ++ [last filtered]


{-
Link to the Kata: https://www.codewars.com/kata/52742f58faf5485cae000b9a

DESCRIPTION:
Your task in order to complete this Kata is to write a function which formats a duration, given as a number of seconds, in a human-friendly way.

The function must accept a non-negative integer. If it is zero, it just returns "now". Otherwise, the duration is expressed as a combination of years, days, hours, minutes and seconds.

It is much easier to understand with an example:

* For seconds = 62, your function should return 
    "1 minute and 2 seconds"
* For seconds = 3662, your function should return
    "1 hour, 1 minute and 2 seconds"
For the purpose of this Kata, a year is 365 days and a day is 24 hours.

Note that spaces are important.

Detailed rules
The resulting expression is made of components like 4 seconds, 1 year, etc. In general, a positive integer and one of the valid units of time, separated by a space. The unit of time is used in plural if the integer is greater than 1.

The components are separated by a comma and a space (", "). Except the last component, which is separated by " and ", just like it would be written in English.

A more significant units of time will occur before than a least significant one. Therefore, 1 second and 1 year is not correct, but 1 year and 1 second is.

Different components have different unit of times. So there is not repeated units like in 5 seconds and 1 second.

A component will not appear at all if its value happens to be zero. Hence, 1 minute and 0 seconds is not valid, but it should be just 1 minute.

A unit of time must be used "as much as possible". It means that the function should not return 61 seconds, but 1 minute and 1 second instead. Formally, the duration specified by of a component must not be greater than any valid more significant unit of time.

-}