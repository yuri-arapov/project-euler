#!/usr/bin/python
# 
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=26
#
# Problem 26
# 13 September 2002
# 
# A unit fraction contains 1 in the numerator. The decimal representation of
# the unit fractions with denominators 2 to 10 are given:
# 
#     1/2  =       0.5
#     1/3  =       0.(3)
#     1/4  =       0.25
#     1/5  =       0.2
#     1/6  =       0.1(6)
#     1/7  =       0.(142857)
#     1/8  =       0.125
#     1/9  =       0.(1)
#     1/10 =       0.1
# 
# Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be
# seen that 1/7 has a 6-digit recurring cycle.
# 
# Find the value of d < 1000 for which 1/d contains the longest recurring cycle
# in its decimal fraction part.
#
# Answer:
#      983
#
# This is grimbal's algorithm
# (see http://projecteuler.net/index.php?section=forum&id=26&page=1)


maxn, maxlen = 0, 0

#for n in range(2, 1001):
for n in range(2, 8):
        print n, "***"
        rest = 1
        for i in range(0, n):
                rest = (rest*10) % n
                print rest

        r0 = rest
        len = 0
        print "***"
        while 1:
                print rest
                rest = (rest*10) % n
                len += 1
                if rest == r0:
                        break

        if len > maxlen:
                maxn = n
                maxlen = len;

        print

print "%d: %d" % (maxn, maxlen)


# end of file
