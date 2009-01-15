#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=24
#
# Problem 24
# 16 August 2002
#
# A permutation is an ordered arrangement of objects. For example, 3124 is one
# possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
# are listed numerically or alphabetically, we call it lexicographic order. The
# lexicographic permutations of 0, 1 and 2 are:
#
# 012   021   102   120   201   210
#
# What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4,
# 5, 6, 7, 8 and 9?
#
# Answer:
#      2783915460

import sys

# http://en.wikipedia.org/wiki/Permutations#Lexicographical_order_generation
# 
# Lexicographical order generation
#
# For every number k, with 0 ≤ k < n!, the following algorithm generates the
# corresponding lexicographical permutation of the initial sequence sj, j= 1...n:
#
#  function permutation(k, s) {
#       var int n:= length(s); factorial:= 1;
#       for j= 2 to n- 1 {             // compute (n- 1)!
#               factorial:= factorial* j;
#       }
#       for j= 1 to n- 1 {
#               tempj:= (k/ factorial) mod (n+ 1- j);
#               temps:= s[j+ tempj]
#               for i= j+ tempj to j+ 1 step -1 {
#                       s[i]:= s[i- 1];      // shift the chain right
#               }
#               s[j]:= temps;
#               factorial:= factorial/ (n- j);
#       }
#       return s;
# }
#
# Notation
#
# * k / j denotes integer division of k by j, i.e. the integral quotient
#   without any remainder, and
# * k mod j is the remainder following integer division of k by j.
def permutation(k, s):
        n = len(s)-1 # '-1' is to ignore first (0-) element

        # compute (n-1)!
        factorial = 1
        for j in range(2, n): # [2, n-1]
                factorial = factorial * j

        for j in range(1, n): # [1, n-1]
                tempj = (k / factorial) % (n + 1 - j)
                temps = s[j + tempj]
                for i in range(j+tempj, j, -1): # [i+tempj, j+1]
                        s[i] = s[i-1]
                s[j] = temps
                factorial = factorial / (n-j)
        return s
# end of permutation


for p in [999999, 1000000, 1000001]:
        s = [0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        permutation(p, s)
        for i in range(1, len(s)):
                sys.stdout.write("%d" % (s[i]));
        sys.stdout.write("\n")

# end of file
