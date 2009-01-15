#!/usr/bin/python
# 
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=16
#
# Problem 16
# 03 May 2002
#
# 215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
#
#                                              1000
# What is the sum of the digits of the number 2     ?
#
# Answer:
#      1366
#
# Long version (no use of built-in large integers)


import sys
import math


Nbits = 1000


Ndigits = int(Nbits / math.log(10, 2)) + 1
# number of decimal digits needed to store
# ("+ 1" is to round to upper bound)
#
#  1000
# 2      (i.e. 1000 bit number)
#
# math:
#    1000     x
#   2     = 10
#
#   where x is number of decimal digits
#
#        1000         x
#   log 2     = log 10        ->
#      2           2
#
#                x
#   1000 = log 10             ->
#             2
#
#
#   1000 = x log 10           ->
#               2
#
#         1000
#   x = --------
#        log 10
#           2


digits = [0] * Ndigits
# array to store decimal digits


#          1000
# compute 2
digits[0] = 1

for i in range(Nbits):
        p = 0
        for d in range(Ndigits):
                p = digits[d] * 2 + p
                digits[d] = p % 10
                p = p / 10


# compute sum of the digits
s = 0
for d in digits:
        s += d


# print result
print s


# end of file
