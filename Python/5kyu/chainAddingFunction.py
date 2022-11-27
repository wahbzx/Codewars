class Add(int):
    def __call__(self,v):
        return Add(self+v)

def add(v):
    return Add(v)

"""
Link to the Kata: https://www.codewars.com/kata/539a0e4d85e3425cb0000a88/python

Description:

We want to create a function that will add numbers together when called in succession.

add(1)(2) # equals 3
We also want to be able to continue to add numbers to our chain.

add(1)(2)(3) # 6
add(1)(2)(3)(4); # 10
add(1)(2)(3)(4)(5) # 15
and so on.

A single call should be equal to the number passed in.

add(1) # 1
We should be able to store the returned values and reuse them.

addTwo = add(2)
addTwo # 2
addTwo + 5 # 7
addTwo(3) # 5
addTwo(3)(5) # 10
We can assume any number being passed in will be valid whole number.
"""
