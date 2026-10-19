program prime_factors
    implicit none
    integer :: n, i

    ! Read the number from the user
    write(*, *) "Enter a number:"
    read(*, *) n

    ! Find and print the prime factors
    write(*, *) "Prime factors of", n, ":"
    do i = 2, n
        do while (mod(n, i) == 0)
            write(*, *) i
            n = n / i
        end do
    end do

    stop
end program prime_factors
