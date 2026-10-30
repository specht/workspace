void main() {
    int number = Integer.parseInt(IO.readln("Enter a number: "));

    IO.print("Prime factors of " + number + ": ");
    for (int i = 2; i <= number; i++) {
        while (number % i == 0) {
            IO.print(i + " ");
            number /= i;
        }
    }
    IO.println();
}
