// Largest palindrome product
//
// Problem 4
// A palindromic number reads the same both ways. The largest palindrome made from
// the product of two 2-digit numbers is 9009 = 91 × 99.
//
// Find the largest palindrome made from the product of two 3-digit numbers.
//
// Answer:  906609

package main

import "fmt"

func NumberToDigits(n int64) []int {
	if n == 0 {
		return []int{0}
	}
	res := []int{}
	for n > 0 {
		res = append(res, int(n%10))
		n = n / 10
	}
	return res
}

func IsPalindromicDigits(n []int) bool {
	l := len(n)
	for i := 0; i < l/2; i++ {
		if n[i] != n[l-i-1] {
			return false
		}
	}
	return true
}

func IsPalindromicNumber(n int64) bool {
	return IsPalindromicDigits(NumberToDigits(n))
}

func foo(n int64) {
	digits := NumberToDigits(n)
	fmt.Println("n", n, "digits", digits, "is palindromic", IsPalindromicDigits(digits))
}

func main() {
	//	foo(12345)
	//	foo(1234554321)
	const maxMult = 999
	const minMult = 100
	count := 0
	var maxPalindromic int64
	for n1 := maxMult; n1 >= minMult; n1-- {
		if int64(n1*maxMult) <= maxPalindromic {
			break // no point in continuing to loop
		}
		for n2 := maxMult; n2 >= minMult; n2-- {
			x := int64(n1 * n2)
			if x <= maxPalindromic {
				break // no point in continuing to loop
			}
			if IsPalindromicNumber(x) {
				maxPalindromic = x
				count++
				fmt.Println(count, x)
			}
		}
	}
}
