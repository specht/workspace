using System;

class BubbleSort
{
    static void Main()
    {
        int[] numbers = new int[10];
        Random random = new Random();

        // Fill the array with random integers
        for (int i = 0; i < numbers.Length; i++)
        {
            numbers[i] = random.Next(1, 100);
        }

        Console.WriteLine("Original array:");
        PrintArray(numbers);

        // Sort the array using bubble sort
        Sort(numbers);

        Console.WriteLine("Sorted array:");
        PrintArray(numbers);
    }

    static void Sort(int[] arr)
    {
        int n = arr.Length;
        for (int i = 0; i < n - 1; i++)
        {
            for (int j = 0; j < n - i - 1; j++)
            {
                if (arr[j] > arr[j + 1])
                {
                    // Swap arr[j] and arr[j+1]
                    int temp = arr[j];
                    arr[j] = arr[j + 1];
                    arr[j + 1] = temp;
                }
            }
        }
    }

    static void PrintArray(int[] arr)
    {
        foreach (int num in arr)
        {
            Console.Write(num + " ");
        }
        Console.WriteLine();
    }
}