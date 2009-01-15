#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=5
#
# Problem 5
# 14 December 2001
# 
# 2520 is the smallest number that can be divided by each of the
# numbers from 1 to 10 without any remainder.
# 
# What is the smallest number that is evenly divisible by all of
# the numbers from 1 to 20?
#
# Answer:
#      232792560
#
# Done

d = 1

for i in range(1, 21):
        y = d
        x = i
        p = 2 # prime

        # this loop splits x into primes
        while x != 1:
                while x % p == 0:
                        # x is divisible by prime p evenly
                        x /= p
                        if y % p == 0:
                                # y is divisible by p as well
                                y /= p
                        else:
                                d *= p
                p += 1

print d

# end of file
