using System;

class Factor
{
    static void Main()
    {
        Console.Write("Enter a number: ");
        int number = int.Parse(Console.ReadLine());

        Console.Write("Prime factors: ");
        for (int i = 2; i <= number; i++)
        {
            while (number % i == 0)
            {
                Console.Write(i + " ");
                number /= i;
            }
        }

        Console.WriteLine();
    }
}