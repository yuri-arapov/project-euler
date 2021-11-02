package factor

// Factor the number.

// Prime number generator.
type PNG struct {
	primes []int64
	index  int
}

// New prime number generator.
func NewPNG() PNG {
	return PNG{
		primes: []int64{2, 3, 5, 7, 11, 13},
		index:  0,
	}
}

// Number of primes computed so far.
func (png *PNG) Count() int {
	return len(png.primes)
}

// Start from the first prime.
func (png *PNG) Reset() {
	png.index = 0
}

// Return current prime number.
func (png *PNG) Current() int64 {
	return png.primes[png.index]
}

// Shift to the next prime number.
func (png *PNG) Next() int64 {
	png.index++
	if png.index >= len(png.primes) {
		png.computeNextPrime()
	}
	return png.Current()
}

// Expand to the next prime number.
func (png *PNG) computeNextPrime() {
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

// Factor the number.
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
