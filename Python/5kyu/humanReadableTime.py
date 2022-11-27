def make_readable(seconds):
    hours = seconds//3600
    if hours> 0:
        seconds-= hours*3600
    minutes = seconds//60
    if minutes>0:
        seconds-= minutes*60
    return "%02i:%02i:%02i" % (hours,minutes,seconds)


"""
Link to the Kata: https://www.codewars.com/kata/52685f7382004e774f0001f7/python

Description:

Write a function, which takes a non-negative integer (seconds) as input and returns the time in a human-readable format (HH:MM:SS)

HH = hours, padded to 2 digits, range: 00 - 99
MM = minutes, padded to 2 digits, range: 00 - 59
SS = seconds, padded to 2 digits, range: 00 - 59
The maximum time never exceeds 359999 (99:59:59)

You can find some examples in the test fixtures.

"""
