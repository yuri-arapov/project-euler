#!/usr/bin/python
# 
# Projet Euler
#
# http://projecteuler.net/index.php?section=problems&id=21
#
# Problem 21
# 05 July 2002
#
# Let d(n) be defined as the sum of proper divisors of n (numbers less than n
# which divide evenly into n).
#
# If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and
# each of a and b are called amicable numbers.
#
# For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44,
# 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4,
# 71 and 142; so d(284) = 220.
#
# Evaluate the sum of all the amicable numbers under 10000.
#
# Answer:
#      31626
#


from math import sqrt


def D(x):
# compute sum of all divisors of the x,
# including 1, but excluding the x
#
# improved version (euler's advise, see forum)
        s = 1
        d = 2
        X = sqrt(x)+1
        while d < X:
                if x % d == 0:
                        # d is x's divisor
                        s += d
                        # x/d is divisor as well
                        if d*d < x:
                                # d is not square root of x
                                s += x/d
                d += 1
        return s
# end of D



s, x = 0, 1
while x < 10000:
        y = D(x)
        if y != x:
                z = D(y)
                if z == x:
                        s += x + y
                        print "found:", x, y
        x += 1

print s/2
# divide by two because each pair is counted twice

# end of file
