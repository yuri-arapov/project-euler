#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=9
# 
# Problem 9
# 25 January 2002
#
# A Pythagorean triplet is a set of three natural numbers, a<b<c, for which,
# a² + b² = c²
#
# For example, 3² + 4² = 9 + 16 = 25 = 5².
#
# There exists exactly one Pythagorean triplet for which a + b + c = 1000.
# Find the product abc.
#
# Answer:
#      31875000
#
# Euclid formula to compute Pythagorean triple found here:
#   http://en.wikipedia.org/wiki/Pythagorean_triple


def p9():
        m, n = 2, 1
        while 1:
                if m % 100 == 0:
                        print m # just to see we're not dead yet

                for n in range(1, m): # IMPORTANT: n < m
                        a, b, c = m*m-n*n, 2*m*n, m*m+n*n # Euclid formula
                        if a+b+c == 1000:
                                print "m=%d, n=%d, a=%d b=%d c=%d, abc=%d" % (m, n, a, b, c, a*b*c)
                                return 
                m += 1
# end of p9


p9()


# end of file
