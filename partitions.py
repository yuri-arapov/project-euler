#!/usr/bin/python
#
# program which computes partitions using the Pentagonal number theorem
# http://en.wikipedia.org/wiki/Pentagonal_number_theorem

pentagonal = lambda n : n*(3*n-1)/2

def generalised_pentagonal(n): # 0, 1, -1, 2, -2
    if n < 0: return 0
    if n%2 == 0: return pentagonal(n/2+1)
    else: return pentagonal(-(n/2+1))

pt = [1]
for n in range (1, 1000+1):
    r = 0
    f = -1
    i = 0
    while generalised_pentagonal(i) <= n:
        k = generalised_pentagonal(i)
        if k > n:
            break
        if i%2==0: f = -f
        r += f*pt[n - k]
        i += 1
    pt.append(r)

print pt


# end of file
