#!/usr/bin/python
#
# 05 Jan, 2008
#
# Project Euler
#
# user: yuridichesky
#
# Problem 1.
#
# http://projecteuler.net/index.php?section=problems&id=1
#
# Find the sum of all the multiples of 3 or 5 below 1000.
#
# If we list all the natural numbers below 10 that are
# multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
# multiples is 23.
#
# Answer:
#      233168
#
# Done.
#


def sum_of_multipless(multiplier,
                      upper_bound): # excluded
### FIXME: 
        N = (upper_bound - 1) / multiplier
        halfN = N / 2
        sum = (multiplier + multiplier*N) * halfN
        if N % 2 == 1:
                sum += multiplier * (halfN + 1)
        return sum
# end of sum_of_multiples


# main
print("%d" % (sum_of_multipless(3, 1000) + 
              sum_of_multipless(5, 1000) - 
              sum_of_multipless(3*5, 1000)))

# end of file
