// Smallest multiple
//
// Problem 5
// 2520 is the smallest number that can be divided by each of the numbers from 1
// to 10 without any remainder.
//
// What is the smallest positive number that is evenly divisible by all of the
// numbers from 1 to 20?
//
// Answer:  232792560
//
// Factor each number from 2 to 20:
// $ ./factor 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
// [2]
// [3]
// [2 2]
// [5]
// [2 3]
// [7]
// [2 2 2]
// [3 3]
// [2 5]
// [11]
// [2 2 3]
// [13]
// [2 7]
// [3 5]
// [2 2 2 2]
// [17]
// [2 3 3]
// [19]
// [2 2 5]
//
// Then get each prime's greatest power (by eye), it's 2^4, 3^2, 5^1, 7^1, 11^1, 13^1, 17^1 and 19^1.
// Then multiply them:
// $ echo '(* 2 2 2 2 3 3 5 7 11 13 17 19)' | guile
// $1 = 232792560

package main

import (
	"fmt"
	"github.com/yuridichesky/project-euler/go/factor"
)

func numberToPrimes(n int64) map[int64]int64 {
	png := factor.NewPNG()
	x := png.Factor(n)
	var y = map[int64]int64{}
	for _, p := range x {
		y[p] = y[p] + 1
	}
	return y
}

func p5(n int64) int64 {
	var primes = map[int64]int64{}
	for i := int64(2); i <= n; i++ {
		pi := numberToPrimes(i)
		for p, pwr := range pi {
			if primes[p] < pwr {
				primes[p] = pwr
			}
		}
	}
	fmt.Printf("p5(%v) primes %v\n", n, primes)
	var res int64 = 1
	for p, pwr := range primes {
		res *= power(p, pwr)
	}
	return res
}

func power(n, pwr int64) int64 {
	var res int64 = 1
	for i := int64(1); i <= pwr; i++ {
		res *= n
	}
	return res
}

func main() {
	for _, n := range []int64{10, 20} {
		fmt.Println(n, p5(n))
	}
}
