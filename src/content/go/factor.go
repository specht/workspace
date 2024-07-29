package main

import (
    "fmt"
    "math"
)

func main() {
    var num int
    fmt.Print("Enter a number: ")
    fmt.Scan(&num)

    fmt.Printf("Prime factors of %d are: ", num)
    for i := 2; i <= int(math.Sqrt(float64(num))); i++ {
        for num%i == 0 {
            fmt.Printf("%d ", i)
            num /= i
        }
    }

    if num > 1 {
        fmt.Printf("%d ", num)
    }
	fmt.Printf("\n");
}