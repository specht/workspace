program bubblesort
    implicit none
    
    integer, parameter :: n = 10
    integer :: array(n)
    real :: realarray(n)
    integer :: i, j, temp
    
    ! Initialize the array with random integers
    call random_seed()
    call random_number(realarray)
    array = floor(realarray * 100)
    
    ! Print the original array
    print *, "Original array:"
    do i = 1, n
        write(*, '(I3)', advance='no') array(i)
    end do
    print *
    
    ! Bubble sort algorithm
    do i = 1, n-1
        do j = 1, n-i
            if (array(j) > array(j+1)) then
                temp = array(j)
                array(j) = array(j+1)
                array(j+1) = temp
            end if
        end do
    end do
    
    ! Print the sorted array
    print *, "Sorted array:"
    do i = 1, n
        write(*, '(I3)', advance='no') array(i)
    end do
    print *
    
end program bubblesort
    