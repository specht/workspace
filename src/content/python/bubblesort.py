import random

def bubble_sort(array):
    for i in range(len(array)):
        for j in range(len(array) - 1):
            if array[j] > array[j + 1]:
                array[j], array[j + 1] = array[j + 1], array[j]

# Create an array of 10 random integers
array = [random.randint(1, 100) for _ in range(10)]

# Print the original array
print("Original array:", array)

# Sort the array using bubble sort
bubble_sort(array)

# Print the sorted array
print("Sorted array:", array)