<div class='meta'>
image: c-logo.png
</div>

# C <span style='font-size: 80%;'>(1972)</span>

<p class='abstract'>
C ist eine imperative Programmiersprache, die 1972 von Dennis Ritchie entwickelt wurde. Sie ist eine der ältesten und einflussreichsten Programmiersprachen und dient als Grundlage für viele andere Programmiersprachen. C ist eine kompilierte Sprache, die eine hohe Leistung und eine enge Kontrolle über die Hardware bietet. Sie wird häufig für die Entwicklung von Betriebssystemen, Treibern, eingebetteten Systemen und Anwendungen mit hoher Leistung verwendet.
</p>

## Hello, World!

```c
#include <stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

## Primfaktorenzerlegung

```c
#include <stdio.h>

int main() {
    int num, i;

    printf("Enter a number: ");
    scanf("%d", &num);

    printf("Prime factors of %d are: ", num);

    for (i = 2; i <= num; i++) {
        while (num % i == 0) {
            printf("%d ", i);
            num /= i;
        }
    }

    return 0;
}
```

## Bubble Sort

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void bubbleSort(int arr[], int n) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}

int main() {
    int arr[10];

    // Generate random numbers
    srand(time(NULL));
    for (int i = 0; i < 10; i++) {
        arr[i] = rand() % 100;
    }

    // Print original array
    printf("Original array: ");
    for (int i = 0; i < 10; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");

    // Sort the array
    bubbleSort(arr, 10);

    // Print sorted array
    printf("Sorted array: ");
    for (int i = 0; i < 10; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");

    return 0;
}
```

<div class='alert alert-warning'>#{stub()}</div>
