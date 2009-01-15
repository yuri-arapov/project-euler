#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=22
#
# Problem 22
# 19 July 2002
#
# Using names.txt (right click and 'Save Link/Target As...'), a 46K text file
# containing over five-thousand first names, begin by sorting it into
# alphabetical order. Then working out the alphabetical value for each name,
# multiply this value by its alphabetical position in the list to obtain a name
# score.
#
# For example, when the list is sorted into alphabetical order, COLIN, which is
# worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
# would obtain a score of 938 x 53 = 49714.
#
# What is the total of all the name scores in the file?
#
# Answer:
#      871198282


def char2id(c):
# return ID of c character, where ID of 'A' is 1, 
# ID of 'B' is 2, etc.
        return 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.index(c) + 1
# end of char2id
#
# NOTE: first I used char2id() function, then I discovered the ord()


names = []


f = open('names', 'r')
while 1:
        n = f.readline()
        if n == "":
                break # end of file
        if n[-1] == '\n':
                n = n[:-1]
        names.append(n)
f.close()


i, S, N, A = 0, 0, len(names), ord('A')
while i < N:
        C = sum((ord(c) - A + 1) for c in names[i])
        S += C * (i+1)
        i += 1

print S

# end of file
