#!/usr/bin/python

a = 1
b = 1
s = 0
while b <= 1000000:
  c = a + b
  a = b
  b = c
  if c % 2 == 0:
    s += c
print s
