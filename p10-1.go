// Summation of primes
//
//  Problem 10
//  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
//
//  Find the sum of all the primes below two million.
//
//  Answer: 142913828922 (brute force)
package main

import (
	"fmt"
)

func main() {
	const N = 2_000_000

	var crossed [N]bool // false - not crossed, true - crossed

	var sum int64

	for n := 2; n < N; {
		sum = sum + int64(n)

		for i := n + n; i < N; i = i + n {
			crossed[i] = true
		}

		for n = n + 1; n < N && crossed[n]; {
			n = n + 1
		}
		// n is either prime or out of range
	}

	fmt.Println(N, sum)
}
