module HaskellGolf where
f n=sum[x*(9*10^(x-1))|x<-[1..toInteger n]]


{-
Link to the Kata: https://www.codewars.com/kata/599f84f86780ef2de7000063

DESCRIPTION:
Task
Given a positive integer n, count the number of digits of all numbers in the range 0 < x < 10^n.

Code Limit
At most 70 characters.

Protip: At least 26 characters will be devoted to other stuff.

Example
f(1) = 9
f(9) = 8888888889
-}