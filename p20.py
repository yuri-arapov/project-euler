#!/usr/bin/python
#
# Project Euler
#
# http://projecteuler.net/index.php?section=problems&id=20
#
# Problem 20
# 21 June 2002
#
# n! means n x (n - 1) x ... x 3 x 2 x 1
#
# Find the sum of the digits in the number 100!
#
# Answer:
#      648


def sum(x, y):
# return x+y, where both x and y are
# text strings representing decimal numbers
# IMPORTANT: x and y must not be empty

        r = ""
        # this is resultant string

        lx, ly = len(x), len(y)
        # the lengths of the input strings

        L = max(lx, ly)
        # length of the longest of input strings

        s = 0
        # initial value of reminder

        for i in range(1, L+1):
                # get a i-digit from each of 
                # x and y starting from the right
                # (i.e. 1 is the rightmost digit)
                if i <= lx:
                        X = int(x[-i]) 
                else: 
                        X = 0
                if i <= ly: 
                        Y = int(y[-i]) 
                else: 
                        Y = 0

                # compute sum of the digits and add
                # a reminder from the previous addition
                R = X + Y + s

                # get a rightmost digit of the sum
                o = R % 10

                # shift the reminder one digit right
                # (i.e. remove rightmost digit from the
                # reminder)
                s = R / 10

                # store given resultant digit at the
                # leftmost position of the resultant
                # string
                r = str(o) + r

        if s > 0:
                r = str(s) + r

        return r
# end of sum


def mul(x, y):
# return x*y, where both x and y are 
# text strings representing decimal numbers
# IMPORTANT: x and y must not be empty

        r = "0"
        # resultant string, must not be empty
        # since it will be passed to sum() function

        for yy in range(1, len(y)+1):
                # walk through all the digits of 'y'

                t = "0" * (yy-1)
                # pad none of zeros at the right for first digit (yy is 1)
                # pad 1 zero at the right for second digit (yy is 2)
                # etc.

                Y = y[-yy]
                # the digit of 'y' to multiply the 'x' by
                # (starting from the right, i.e. first is the
                # rightmost digit)

                s = 0
                # reminder

                for xx in range(1, len(x)+1):
                        # walk through all the digits of 'x'

                        X = x[-xx]
                        # the digit of 'x' to be multiplied by Y
                        # starting from the right (i.e. first digit
                        # is the rightmost one)

                        m = int(Y)*int(X) + s
                        # multiply X*Y and add a reminder

                        o = m % 10
                        # get the rightmost decimal digit of the 'm'

                        s = m / 10
                        # remove the rightmost decimal digit and 
                        # store result as a reminder

                        t = str(o) + t
                        # store resultant decimal digit at the left
                        # of resultant string

                if s > 0:
                        t = str(s) + t

                r = sum(r, t)
                # accumulate the result
        return r
# end of mul


# print "99 + 99 =", sum("99", "99")
# print "99 * 99 =", mul("99", "99")


f = "1"
for n in range(2, 101):
        f = mul(f, str(n))

print "100! = ", f


s = 0
for i in f:
        s += int(i)


print "the sum of 100! digits is", s

# end of file
