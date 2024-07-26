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
