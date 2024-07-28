program prime_factors
    implicit none
    integer :: n, i

    ! Read the number from the user
    write(*, *) "Enter a number:"
    read(*, *) n

    ! Find and print the prime factors
    write(*, *) "Prime factors of", n, ":"
    do i = 2, n
        if (mod(n, i) == 0) then
            write(*, *) i
            n = n / i
        end if
    end do

    stop
end program prime_factors
