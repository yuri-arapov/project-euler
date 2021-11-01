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
	var a int = 2
	var b, c int
Loop:
	for ; ; a++ {
		b = a + 1
		for ; ; b++ {
			c = 1000 - (a + b) // a+b+c=1000
			if c <= b {
				break
			}
			// a<b<c
			if a*a+b*b == c*c {
				break Loop
			}
		}
	}
	fmt.Println(a, b, c, a*b*c)
}
