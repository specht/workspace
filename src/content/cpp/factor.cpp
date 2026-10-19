#include <iostream>
#include <vector>

using namespace std;

int main() {
    int number;
    cout << "Enter a number: ";
    cin >> number;

    // 0, a negative number or no number at all: 0 % 2 == 0 forever
    if (number < 2) {
        cout << "Please enter a number of at least 2." << endl;
        return 1;
    }

    cout << "Prime factors of " << number << " are: ";
    while (number % 2 == 0) {
        cout << 2 << " ";
        number /= 2;
    }

    for (int i = 3; i * i <= number; i += 2) {
        while (number % i == 0) {
            cout << i << " ";
            number /= i;
        }
    }

    if (number > 2)
        cout << number;

    cout << endl;

    return 0;
}