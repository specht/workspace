# Hackschule Workspace

- login with invitation only
- user node gets created on first login

- `fs_tag`: derived from email
  - used for user data directory
  - never published
- `server_tag`:
  - random tag
  - stored in user node
  - used in url
  - created on user creation
  - changed on server reset for cache busting
  - stored in browser cookie
    - if change is detected => purge indexeddb
- `server_sid`:
  - random tag
  - stored in user node
  - sent to user on login
  - must be sent to already logged in user if not present
- `share_tag`:
  - pretty long, random tag
  - stored in user node
  - max. 1 per user
  - can be removed again (stop sharing)
  - used to share access with other user
  - used in nginx config (requires no sid)

tic-80
mysql
Ruby
Neo4j

DBML:

nicolas-liger.dbml-viewer
bocovo.dbml-erd-visualizer


=> Fortran gfortran
apt install gfortran
program hello
    print *, "Hello, World!"
end program hello
gfortran hello.f90 -o hello

https://www.mjr19.org.uk/IT/sorts/
https://rosettacode.org/wiki/Sorting_algorithms/Bubble_sort#Fortran

module sorts
  implicit none
  integer, parameter :: prec=kind(1d0), i64=selected_int_kind(15)

  ! array L used by smoothsort


contains
  subroutine bubble_sort(array)

    real(prec), intent(inout) :: array(:)
    real(prec) :: temp
    integer :: i,j,last

    last=size(array)
    do i=last-1,1,-1
       do j=1,i
          if (array(j+1).lt.array(j)) then
             temp=array(j+1)
             array(j+1)=array(j)
             array(j)=temp
          endif
       enddo
    enddo

  end subroutine bubble_sort
end module sorts

! Code to perform trivial benchmark
! of a collection of sort algorithms
!
! MJR Oct 2019
!
! Nov 2023, ensure random nos are both +ve and -ve

program b
  use sorts
  
  write(*,*)'Bubble sort'
  call bench(bubble_sort,50000)
  write(*,*)

  contains

subroutine bench(sort,big)
  use sorts
  procedure(bubble_sort) :: sort
  real(prec), allocatable :: array(:)
  integer, intent(in) :: big
  integer i,off,t1,t2,rate
  real(prec):: check
  
  allocate(array(big))
  i=10
  ! find rate
  call system_clock(t1,rate)
  write(*,*)'      Size      time/element'
  do while(i<=big)
     call random_number(array)
     array=array-0.2d0
     check=sum(array(1:i))
     off=1
     call system_clock(t1)
     do while(off+i-1<=big)
     call sort(array(off:off+i-1))
        off=off+i
     enddo
     call system_clock(t2)
! Check result is ordered
     do j=2,i
        if (array(j)<array(j-1)) then
           write(*,*)'Sort error at ',j,' value ',array(j)
           if (i.le.100) then
              write(*,*)array(1:i)
           else
              write(*,*)array(j-3:j+3)
           endif
           stop
        endif
     enddo
! Check sum of result is approximately the same as it was
     check=check-sum(array(1:i))
     if (check.gt.1e-15*i*sqrt(real(i))) then
        write(*,*)'Large difference in sums before and after',check
     endif
     
     write(*,*)i,real(t2-t1)/(real(rate)*i*(big/i))
     i=10*i
  enddo
  deallocate(array)
end subroutine bench

end program b

=> BASIC bwbasic
10 FOR I = 1 TO 10
20 PRINT "HELLO, WORLD!", I
30 NEXT I

=> C gcc

=> C++ g++

=> Common Lisp (clisp) .lsp
(defun hello ()
           (format t "Hello, World!"))
(hello)
=> Python

=> Lua
-- hello world lua program
print ("Hello, World!")
lua hello.lua

=> Ruby
=> Java
=> C#
using System;

public class HelloWorld
{
    public static void Main(string[] args)
    {
        Console.WriteLine ("Hello Mono World");
    }
}

mcs hello.cs
mono hello.exe
=> Go

package main
import "fmt"
func main() {
    fmt.Println("hello world")
}

go run hello.go
go build hello.go && ./hello

=> Dart

void main() {
  print("Hello, World!");
}

dart hello.dart

=> Rust

Can you write a simple Hello World program in LANG?

Can you write a program in LANG which reads a number from the user and prints the prime factors of that number?

Can you write a program in LANG which creates an array of 10 random integers, prints the array, sorts the array using the bubble sort algorithm and prints the sorted numbers?