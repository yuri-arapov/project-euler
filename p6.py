#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=6
#
# Problem 6
# 30 November 2001
#
# The sum of the squares of the first ten natural numbers is,
# 1² + 2² + ... + 10² = 385
#
# The square of the sum of the first ten natural numbers is,
# (1 + 2 + ... + 10)² = 55² = 3025
#
# Hence the difference between the sum of the squares of the
# first ten natural numbers and the square of the sum is 3025 −
# 385 = 2640.
#
# Find the difference between the sum of the squares of the
# first one hundred natural numbers and the square of the sum.
#
# Answer:
#      25164150


# brute force version
s1 = 0
s2 = 0

for n in range(1, 101):
        s1 += n*n
        s2 += n

print s2*s2 - s1


# end of file
