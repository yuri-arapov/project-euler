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
# Short version (use of builtin large integers)

s = 0
for d in str(2**1000):
        s += int(d)
print s

# end of file
