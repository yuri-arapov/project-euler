#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=28
#
# Problem 28
# 11 October 2002
#
# Starting with the number 1 and moving to the right in a clockwise direction a
# 5 by 5 spiral is formed as follows:
#
# 21 22 23 24 25
# 20  7  8  9 10
# 19  6  1  2 11
# 18  5  4  3 12
# 17 16 15 14 13
#
# It can be verified that the sum of both diagonals is 101.
#
# What is the sum of both diagonals in a 1001 by 1001 spiral formed in the same
# way?
#
# Answer:
#      669171001
#

d = 1

for n in range(3, 1003, 2):
        d += 4*n**2 - 6*n + 6
        print n, d

print d

# end of file
