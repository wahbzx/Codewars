def ips_between(start, end):
    ip1 = start.split(".")[::-1]
    ip2 = end.split(".")[::-1]
    diffs = []
    for i in range (0,len(ip1)):
        diffs.append(int(ip2[i]) - int(ip1[i]))
    sums = []
    powers = [1,2**8,2**16,2**24]
    for i in range (0, len(diffs)):
        sums.append(diffs[i] * powers[i])
    return (sum(sums))

"""
Link to the Kata: https://www.codewars.com/kata/526989a41034285187000de4/python

Description:

Implement a function that receives two IPv4 addresses, and returns the number of addresses between them (including the first one, excluding the last one).

All inputs will be valid IPv4 addresses in the form of strings. The last address will always be greater than the first one.

Examples
* With input "10.0.0.0", "10.0.0.50"  => return   50 
* With input "10.0.0.0", "10.0.1.0"   => return  256 
* With input "20.0.0.10", "20.0.1.0"  => return  246

"""
