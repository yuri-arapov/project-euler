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
# FIXME: BRUTE FORCE!


d1, d2 = 1, 0


x, y, n, s = 0, 0, 1, 1


while s < 1001:
        s += 2
        x, n = x+1, n+1
        y, n = y+(s-2), n+(s-2)
        # now: x == y

#        if x !=y:
#                print x, y
#                assert x == y

#        print x, y, n

        d1 += n
        x, n = x-(s-1), n+(s-1)
        # now: x == -y

#        if x != -y:
#                print x, y
#                assert x == -y

#        print x, y, n

        d2 += n
        y, n = y-(s-1), n+(s-1)
        # now: x == y
        d1 += n

#        if x != y:
#                print x, y
#                assert x == y

#        print x, y, n

        x, n = x+(s-1), n+(s-1)
        # now: x == -y
        d2 += n

#        if x != -y:
#                print x, y
#                assert x == -y

#        print x, y, n
# 
        print s, n, d1+d2
        

print s, n, d1+d2

# end of file
