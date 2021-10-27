package main

import "fmt"

// Largest prime factor
//
// Problem 3
// The prime factors of 13195 are 5, 7, 13 and 29.
//
// What is the largest prime factor of the number 600851475143 ?
//
// Answer: 6857

type PNG struct {
	primes []int64
	index  int
}

func NewPNG() PNG {
	return PNG{
		primes: []int64{2, 3, 5, 7, 11, 13},
		index:  0,
	}
}

func (png *PNG) Length() int {
	return len(png.primes)
}

func (png *PNG) Reset() {
	png.index = 0
}

func (png *PNG) Current() int64 {
	return png.primes[png.index]
}

func (png *PNG) Next() int64 {
	png.index++
	if png.index >= len(png.primes) {
		png.grow()
	}
	return png.Current()
}

func (png *PNG) grow() {
	n := png.primes[len(png.primes)-1] + 1
	for {
		nIsPrime := true
		for _, p := range png.primes {
			if n%p == 0 {
				nIsPrime = false
				break
			}
		}
		if nIsPrime {
			png.primes = append(png.primes, n)
			break
		}
		n++
	}
}

func (png *PNG) Factor(n int64) []int64 {
	png.Reset()
	res := []int64{}
	for n != 1 {
		p := png.Current()
		for {
			if p*p > n {
				// n is prime
				res = append(res, n)
				n = 1
				break
			}
			if n%p == 0 {
				res = append(res, p)
				n = n / p
			} else {
				png.Next()
				break
			}
		}
	}
	return res
}

func main() {
	fmt.Println("Project Euler")
	fmt.Println("Problem 3")

	png := NewPNG()

	// first 100 primes
	for i := 0; i < 100; i++ {
		fmt.Println(png.Current())
		png.Next()
	}

	// factor some numbers
	for _, n := range []int64{8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 13195, 600851475143} {
		fmt.Println(n, png.Factor(n))
	}

	fmt.Println("number of primes computed:", png.Length())
}
