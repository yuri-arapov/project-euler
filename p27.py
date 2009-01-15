#!/usr/bin/python
#
# Porject Euler
#
# http://projecteuler.net/index.php?section=problems&id=27
#
# Problem 27
# 27 September 2002
# 
# Euler published the remarkable quadratic formula:
# 
# n² + n + 41
# 
# It turns out that the formula will produce 40 primes for the consecutive
# values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is
# divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
# divisible by 41.
# 
# Using computers, the incredible formula  n² − 79n + 1601 was discovered,
# which produces 80 primes for the consecutive values n = 0 to 79. The product
# of the coefficients, −79 and 1601, is −126479.
# 
# Considering quadratics of the form:
# 
#     n² + an + b, where |a| < 1000 and |b| < 1000
# 
#     where |n| is the modulus/absolute value of n
#     e.g. |11| = 11 and |−4| = 4
# 
# Find the product of the coefficients, a and b, for the quadratic expression
# that produces the maximum number of primes for consecutive values of n,
# starting with n = 0.
#
# Answer:
#      -59231
#
# FIXME: BRUTE FORCE!!
# FIXME: see forum for smart solutions:
# FIXME:   http://projecteuler.net/index.php?section=forum&id=27


import math


def is_prime(x):
        X = int(math.sqrt(x)+1)
        d = 2
        while d < X:
                if x % d == 0:
                        return 0 # x is not a prime
                d += 1
        return 1 # x is prime
# end of is_prime


def number_of_primes(a, b):
        N, n = 0, 0
        while 1:
                x = n*n + a*n + b
                if x > 0 and is_prime(x):
                        N += 1
                else:
                        break
                n += 1
        return N
# end of number_of_primes


Limit = 1000
A, B, N = 0, 0, 0


for a in range(-Limit+1, Limit):
        if a % 100 == 0:
                print a
        for b in range(-Limit+1, Limit):
                n = number_of_primes(a, b)
                if n > N:
                        A, B, N = a, b, n
                        print A, B, N

print A*B


# end of file
