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