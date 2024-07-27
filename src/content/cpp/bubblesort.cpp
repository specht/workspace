#include <iostream>
#include <cstdlib>
#include <ctime>

void bubbleSort(int arr[], int size) {
    for (int i = 0; i < size - 1; i++) {
        for (int j = 0; j < size - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}

int main() {
    // Seed the random number generator
    srand(time(0));

    // Create an array of 10 random integers
    int arr[10];
    for (int i = 0; i < 10; i++) {
        arr[i] = rand() % 100; // Generate a random number between 0 and 99
    }

    // Print the original array
    std::cout << "Original array: ";
    for (int i = 0; i < 10; i++) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;

    // Sort the array using bubble sort
    bubbleSort(arr, 10);

    // Print the sorted array
    std::cout << "Sorted array: ";
    for (int i = 0; i < 10; i++) {
        std::cout << arr[i] << " ";
    }
    std::cout << std::endl;

    return 0;
}