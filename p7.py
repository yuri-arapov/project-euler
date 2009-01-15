#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=7
# 
# Problem 7
# 28 December 2001
#
# By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
# that the 6th prime is 13.
#
# What is the 10001st prime number?
# Answer:
#      104743


maxN = 10001  # number (index) of the prime we're looking for

N = 0         # number (count) of primes found so far
primes = []   # list of primes

i = 2         # first number to test for being the prime one


def is_prime_burte_force(x):
#        print "is_prime(%d)" % (x)
        for i in range(2, x): # NB: x is excluded from range
                if x % i == 0:
                        return 0
        return 1
# end of is_prime_burte_force


def is_prime_smart(x):
# any non-prime number is divisible by some
# prime that is less then this number.
#
# so if none of the primes (less then this number)
# divide x evenly, then x is a prime.
#
# and the 'upper border' is a maximum candidate for
# divisor.
        upper_border = x**0.5 + 1
        for p in primes:
                if p >= upper_border:
                        break
                if x % p == 0:
                        return 0
        return 1
# end of is_prime_smart


# main

# map test function
is_prime = is_prime_smart

while 1:
        if is_prime(i):
                primes.append(i)
                N += 1
                if N <= 10 or N % 100 == 0 or N == maxN:
                        print "%6d is %dth prime" % (i, N)
        if N == maxN:
                break
        i += 1

# end of file
