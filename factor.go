package main

import (
	"fmt"
	"github.com/yuridichesky/project-euler/go/factor"
	"os"
	"strconv"
)

// Factor the number.

func main() {

	png := factor.NewPNG()

	for _, arg := range os.Args[1:] {
		n, err := strconv.ParseInt(arg, 10, 64)
		if err != nil {
			fmt.Println(err)
			continue
		}
		fmt.Println(png.Factor(n))
	}
}
