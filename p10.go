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
	"github.com/yuridichesky/project-euler/go/factor"
)

func main() {
	png := factor.NewPNG()
	var sum int64
	count := 0
	for {
		if png.Current() >= 2_000_000 {
			break
		}
		sum += png.Current()
		count++
		if count%100 == 0 {
			// progress indicator
			fmt.Println(count, png.Current(), sum)
		}
		png.Next()
	}
	fmt.Println(sum)
}
