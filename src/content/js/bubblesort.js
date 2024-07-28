// Generate an array of 10 random integers
const array = [];
for (let i = 0; i < 10; i++) {
    array.push(Math.floor(Math.random() * 100));
}

// Print the original array
console.log("Original array:", array);

// Bubble sort function
function bubbleSort(arr) {
    for (let i = 0; i < arr.length - 1; i++) {
        for (let j = 0; j < arr.length - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                // Swap elements
                const temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}

// Sort the array using bubble sort
bubbleSort(array);

// Print the sorted array
console.log("Sorted array:", array);