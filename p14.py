#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=14
#
# Problem 14
# 05 April 2002
#
# The following iterative sequence is defined for the set of positive integers:
#
#   n -> n/2    (n is even)
#   n -> 3n + 1 (n is odd)
#
# Using the rule above and starting with 13, we generate the following sequence:
# 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
#
# It can be seen that this sequence (starting at 13 and finishing at 1)
# contains 10 terms. Although it has not been proved yet (Collatz Problem), it
# is thought that all starting numbers finish at 1.
#
# Which starting number, under one million, produces the longest chain?
#
# NOTE: Once the chain starts the terms are allowed to go above one million.
#
# Answer:
#      837799


import sys


start = 1000000 - 1 # first number under one million
max_start = start


def next(n):
        if n % 2 == 0:
                return n / 2
        else:
                return 3*n + 1
# end of next()


def sequence_length(s):
        global max_start
        n = 1
        while s > 1:
                s = next(s)
                if s > max_start:
                        max_start = s
                        sys.stdout.write("\n*** %d\n" % (max_start))
                n += 1
        return n
# end of sequence_length


# main

N, res = 0, start


while start > 0:
        if start % 100 == 0:
                sys.stdout.write("%8d\r" % (start))
                sys.stdout.flush()
        n = sequence_length(start)
        if n > N:
                sys.stdout.write("\n%d %d\n" % (start, n))
                sys.stdout.flush()
                N, res = n, start
        start -= 1


# end of file
