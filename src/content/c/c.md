<div class='meta'>
image: c-logo.png
</div>

<div class='floatright mt-5' style='width: 12em;'>
    <img src='ritchie.webp'>
    <p>Dennis Ritchie</p>
</div>

# C <span style='font-size: 80%;'>(1972)</span>

<p class='abstract'>
C ist eine imperative Programmiersprache, die 1972 von Dennis Ritchie bei Bell Labs entwickelt wurde. Sie ist eine der ältesten und einflussreichsten Programmiersprachen und dient als Grundlage für viele andere Programmiersprachen. C ist eine kompilierte Sprache, die eine hohe Leistung und eine enge Kontrolle über die Hardware bietet. Sie wird häufig für die Entwicklung von Betriebssystemen, Treibern, eingebetteten Systemen und Anwendungen mit hoher Leistung verwendet.
</p>

## Eigenschaften

- **Imperative Programmiersprache**: C ist eine imperative Programmiersprache, die auf der sequentiellen Ausführung von Anweisungen basiert.
- **Kompilierte Sprache**: C ist eine kompilierte Sprache, was bedeutet, dass der Code in Maschinencode übersetzt wird, bevor er ausgeführt wird.
- **Hohe Leistung**: C ist eine der schnellsten Programmiersprachen und wird häufig für rechenintensive Anwendungen eingesetzt.
- **Hardware-Nähe**: C bietet eine enge Kontrolle über die Hardware und ermöglicht es, direkt mit Speicheradressen und Registern zu arbeiten.
- **Portabilität**: C-Code ist in der Regel portabel und kann auf verschiedenen Plattformen und Betriebssystemen ausgeführt werden.
- **Modularität**: C unterstützt die modulare Programmierung, was es ermöglicht, den Code in separate Module oder Dateien aufzuteilen.
- **Standardbibliothek**: C verfügt über eine umfangreiche Standardbibliothek, die eine Vielzahl von Funktionen und Datentypen bereitstellt.

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
