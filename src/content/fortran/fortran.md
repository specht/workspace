<div class='meta'>
image: fortran-logo.png
</div>

# Fortran <span style='font-size: 80%;'>(1957)</span>

<p class='abstract'>
Fortran wurde 1957 von IBM entwickelt und ist eine der ältesten Programmiersprachen. Sie wurde ursprünglich für wissenschaftliche und technische Anwendungen entwickelt und wird heute noch in diesen Bereichen eingesetzt. Fortran ist eine imperative Programmiersprache, die speziell für numerische Berechnungen optimiert ist. Sie bietet eine Vielzahl von Funktionen und Bibliotheken für mathematische und wissenschaftliche Berechnungen.
</p>

## Hello, world!

```fortran
program HelloWorld
    print *, "Hello, World!"
end program HelloWorld
```

## Primfaktorenzerlegung

```fortran
program prime_factors
    implicit none
    integer :: n, i

    ! Read the number from the user
    write(*, *) "Enter a number:"
    read(*, *) n

    ! Find and print the prime factors
    write(*, *) "Prime factors of", n, ":"
    do i = 2, n
        if (n % i == 0) then
            write(*, *) i
            n = n / i
            i = i - 1
        end if
    end do

    stop
end program prime_factors
```

## Bubble Sort

```fortran
program bubble_sort
    implicit none
    integer, parameter :: n = 10
    integer :: arr(n)
    integer :: i, j, temp

    ! Initialize the array with random values
    call random_seed()
    call random_number(arr)

    ! Print the unsorted array
    print *, "Unsorted array:"
    do i = 1, n
        print *, arr(i)
    end do

    ! Call the bubble sort function
    call bubble_sort(arr, n)

    ! Print the sorted array
    print *, "Sorted array:"
    do i = 1, n
        print *, arr(i)
    end do

contains

    subroutine bubble_sort(arr, n)
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: n
        integer :: i, j, temp

        do i = 1, n-1
            do j = 1, n-i
                if (arr(j) > arr(j+1)) then
                    temp = arr(j)
                    arr(j) = arr(j+1)
                    arr(j+1) = temp
                end if
            end do
        end do

    end subroutine bubble_sort

end program bubble_sort
```

<div class='alert alert-warning'>#{stub()}</div>
