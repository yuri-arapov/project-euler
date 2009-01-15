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


size = 1000000
seq_len = [0] * size
max_start, max_len = 1, 0

max_j = 0

i = 1
while i < size:
        j, n = i, 1

        while j > 1:
                if j > max_j:
                        print j # just to see how large the numbers may be
                        max_j = j

                if j < i and seq_len[j] > 0:
                        # print "*** %d %d" % (j, seq_len[j])
                        n += seq_len[j]
                        break

                if j % 2 == 0:
                        j = j/2
                else:
                        j = j*3 + 1

                n += 1
        # while

        seq_len[i] = n

        if n > max_len:
                max_start, max_len = i, n

        i += 1
# while

print "%d %d" % (max_start, max_len)

# end of file
