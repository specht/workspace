#include <iostream>
#include <vector>

using namespace std;

int main() {
    int number;
    cout << "Enter a number: ";
    cin >> number;

    cout << "Prime factors of " << number << " are: ";
    while (number % 2 == 0) {
        cout << 2;
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