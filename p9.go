// Special Pythagorean triplet
//
//  Problem 9
//  A Pythagorean triplet is a set of three natural numbers, a < b < c, for
//  which,
//
//  a^2 + b^2 = c^2
//  For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.
//
//  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
//  Find the product abc.
//
//  Answer: 31875000
package main

import "fmt"

func main() {
	var a, b, c int
	a = 2
Loop:
	for ; ; a++ {
		b = a + 1
		for ; ; b++ {
			c = 1000 - (a + b)
			if c <= b {
				break
			}
			if a*a+b*b == c*c {
				break Loop
			}
		}
	}
	fmt.Println(a, b, c, a*b*c)
}
