package main

import "fmt"

func main() {
	sum := 0
	fib := 1
	fibPrev := 0
	const maxFib = 4_000_000
	for {
		if fib > maxFib {
			break
		}
		if fib%2 == 0 {
			sum += fib
		}
		fib, fibPrev = fib+fibPrev, fib
	}
	fmt.Println(sum)
}
