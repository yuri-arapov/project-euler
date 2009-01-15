#!/usr/bin/python
#
# Problem Euler
#
# http://projecteuler.net/index.php?section=problems&id=15
#
# Problem 15
# 19 April 2002
#
# Starting in the top left corner of a 2×2 grid, there are 6 routes (without
# backtracking) to the bottom right corner.
#
# How many routes are there through a 20×20 grid?
# Answer:
#      137846528820

RR = []
for r in range(21):
        RR.append([0]*21)

def R(r, c):
# r stands for 'rows'
# c stands for 'columns'
        if RR[r][c] > 0:
                r=r  # empty operator
        elif r == 1: # just one row
                RR[r][c] = c+1
        elif c == 1: # just one column
                RR[r][c] = r+1
        else: 
                RR[r][c] = R(r-1, c) + R(r, c-1)
        print "%2d %2d -> %d" % (r, c, RR[r][c])
        return RR[r][c]
# end of R

print R(20, 20)

# end of file
