#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=17
#
# Problem 17
# 17 May 2002
#
# If the numbers 1 to 5 are written out in words: one, two, three, four, five;
# there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
#
# If all the numbers from 1 to 1000 (one thousand) inclusive were written out
# in words, how many letters would be used?
#
# NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
# forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
# letters.
#
# Answer:
#      21124


one_twenty = [
        "<dummy>",
        "one",
        "two",
        "three",
        "four",
        "five",
        "six",
        "seven",
        "eight",
        "nine",
        "ten",
        "eleven",
        "twelve",
        "thirteen",
        "fourteen",
        "fifteen",
        "sixteen",
        "seventeen",
        "eighteen",
        "nineteen",
        "twenty"]


twenty_ninety = [
        "twenty",
        "thirty",
        "forty",
        "fifty",
        "sixty",
        "seventy",
        "eighty",
        "ninety"]


for i in range(1, 1000):
        n = i
        
        s = ""
        s10 = ""

        if n >= 100:
                s = one_twenty[n / 100] + " hundred"
                n = n % 100
                if n > 0:
                        s += " and "

        if 1 <= n and n <= 20:
                s10 = one_twenty[n]
        
        if n > 20:
                d = n / 10
                o = n % 10
                s10 = twenty_ninety[d-2]
                if o > 0:
                        s10 += "-" + one_twenty[o]
        
        s += s10

        print "%4d %s" % (i, s)

print "%4d one thousand" % (1000)


# end of file
