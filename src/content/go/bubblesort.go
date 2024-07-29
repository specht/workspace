package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // Seed the random number generator
    rand.Seed(time.Now().UnixNano())

    // Create an array of 10 random integers
    numbers := make([]int, 10)
    for i := 0; i < 10; i++ {
        numbers[i] = rand.Intn(100)
    }

    // Print the original array
    fmt.Println("Original array:", numbers)

    // Sort the array using bubble sort
    bubbleSort(numbers)

    // Print the sorted array
    fmt.Println("Sorted array:", numbers)
}

func bubbleSort(numbers []int) {
    n := len(numbers)
    for i := 0; i < n-1; i++ {
        for j := 0; j < n-i-1; j++ {
            if numbers[j] > numbers[j+1] {
                // Swap numbers[j] and numbers[j+1]
                numbers[j], numbers[j+1] = numbers[j+1], numbers[j]
            }
        }
    }
}