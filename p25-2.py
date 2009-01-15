#!/usr/bin/python
#
# Project Euler
#
# Problem 25
# 30 August 2002
# 
# The Fibonacci sequence is defined by the recurrence relation:
# 
#     Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
# 
# Hence the first 12 terms will be:
# 
#     F1 = 1
#     F2 = 1
#     F3 = 2
#     F4 = 3
#     F5 = 5
#     F6 = 8
#     F7 = 13
#     F8 = 21
#     F9 = 34
#     F10 = 55
#     F11 = 89
#     F12 = 144
# 
# The 12th term, F12, is the first term to contain three digits.
# 
# What is the first term in the Fibonacci sequence to contain 1000 digits?
#
# Answer:
#      4781
#
# Binet's Fibonacci number formula:
#
#        [      n  ]
#        [   phi   ]
#   F  = [ ------- ]
#    n   [ sqrt(5) ]
#
#
#                 sqrt(5)+1
#   where phi is  ---------  (golden ratio)
#                     2
#
#       999
# F > 10    (i.e. it contains 1000 and more digit)
#  x
#
#       x
#    phi       999
#  ------- > 10             -> log (base 10) left and right sides
#  sqrt(5)
#
#         x
#  log(phi ) - log(sqrt(5)) > 999    ->
#
#
#  x * log(phi) - log(sqrt(5)) > 999 ->
#
#
#      999 + log(sqrt(5))
#  x > ------------------
#           log(phi)



from math import sqrt, log, floor


phi = (sqrt(5)+1)/2.0

x = int( floor( (999 + log(sqrt(5), 10)) / log(phi, 10) ) )

print x

# end of file
