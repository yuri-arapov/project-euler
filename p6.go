// Sum square difference
//
//  Problem 6
//  The sum of the squares of the first ten natural numbers is,
//   1^2 + 2^2 + ...+ 10^2 = 385
//
//  The square of the sum of the first ten natural numbers is,
//   (1 + 2 + ... + 10)^2 = 55^2 = 3025
//
//  Hence the difference between the sum of the squares of the first ten
//  natural numbers and the square of the sum is .
//
//  Find the difference between the sum of the squares of the first one hundred
//  natural numbers and the square of the sum.
//
//  Answer: 25164150
//
package main

import "fmt"

func sumOfSquares(from, to int64) int64 {
	var res int64
	for i := from; i <= to; i++ {
		res += i * i
	}
	return res
}

func squareOfSum(from, to int64) int64 {
	x := (to + from) * ((to - from + 1) / 2)
	return x * x
}

func p6(n int64) {
	s1 := sumOfSquares(1, n)
	s2 := squareOfSum(1, n)
	fmt.Println(n, s1, s2, s2-s1)
}

func main() {
	p6(10)
	p6(100)
}
