#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=23
#
# Problem 23
# 02 August 2002
#
# A perfect number is a number for which the sum of its proper divisors is
# exactly equal to the number. For example, the sum of the proper divisors of
# 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
#
# A number whose proper divisors are less than the number is called deficient
# and a number whose proper divisors exceed the number is called abundant.
#
# As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest
# number that can be written as the sum of two abundant numbers is 24. By
# mathematical analysis, it can be shown that all integers greater than 28123
# can be written as the sum of two abundant numbers. However, this upper limit
# cannot be reduced any further by analysis even though it is known that the
# greatest number that cannot be expressed as the sum of two abundant numbers
# is less than this limit.
#
# Find the sum of all the positive integers which cannot be written as the sum
# of two abundant numbers.
#
# Answer:
#      4179871


def S(x):
# sigma function
# return sum of proper divisors of the x
#
# see:
#   http://mathworld.wolfram.com/DivisorFunction.html
#   http://primes.utm.edu/glossary/page.php?sort=SigmaFunction
        X = x
        s = 1
        d = 2 # first prime
        while X != 1:
                if X % d == 0:
                        po = 0
                        while X % d == 0:
                                X /= d
                                po += 1
                        s *= (d**(po+1) - 1)/(d - 1)
                d += 1
        return s - x # exclude x itself from the sum 
                     # to take into account _proper_ divisors only
# end of S


LIMIT = 28123

AN = [] # list of abundant numbers
x = 12  # first abundant number
while x < LIMIT:
        if S(x) > x:
                AN.append(x)
        x += 1

print "number of abundant numbers under 28123 is", len(AN)

flag = [0] * (LIMIT + 1)

for i in range(len(AN)):
        for j in range(i, len(AN)):
                x = AN[i] + AN[j] # the sum of two abundant numbers
                if x < len(flag):
                        flag[x] = 1

s = 0
for i in range(len(flag)):
        if flag[i] == 0:
                s += i

print s

# end of file
