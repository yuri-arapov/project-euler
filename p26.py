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
# FIXME: BRUTE FORCE!!


def position(x, nn):
        for i in range(0, len(nn)):
                if x == nn[i]:
                        return i
        return -1
# end of position


D, RL = 0, 0

d = 2
while d < 1000:
        n, nn, ii = 1, [], []
        while 1:
                while n < d:
                        n *= 10
                        ii.append(0)

                p = position(n, nn)

                if p > -1: # recurring cycle found
                        rl = len(nn) - p # length of recurring part
                        if rl > RL:
                                D, RL = d, rl
                                print D, RL
                        break
                else:
                        nn.append(n)

                i = n / d
                ii.append(i)
                n = n % d * 10

                if n == 0:
                        break # end of division

        d += 1


# end of file
