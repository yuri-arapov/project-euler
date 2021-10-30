// 10001st prime
//
//  Problem 7
//  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see
//  that the 6th prime is 13.
//
//  What is the 10 001st prime number?
//
//  Answer:  104743
package main

import (
	"fmt"
	"github.com/yuridichesky/project-euler/go/factor"
)

func main() {
	png := factor.NewPNG()
	for png.Count() < 10001 {
		png.Next()
	}
	fmt.Println(png.Current())
}
