section .data
    prompt db "Enter a number: ", 0
    factors db "Prime factors: ", 0
    newline db 10, 0

section .bss
    number resb 10

section .text
    global _start

_start:
    ; Display prompt
    mov eax, 4
    mov ebx, 1
    mov ecx, prompt
    mov edx, 16
    int 0x80

    ; Read number from user
    mov eax, 3
    mov ebx, 0
    mov ecx, number
    mov edx, 10
    int 0x80

    ; Convert number from ASCII to integer
    xor ebx, ebx
    mov bl, byte [number]
    sub bl, '0'

    ; Find prime factors
    mov ecx, 2
    mov edx, 0

find_factors:
    cmp ebx, 1
    je print_factors

    cmp ebx, ecx
    jb next_factor

    div ecx
    cmp edx, 0
    jne next_factor

    ; Print prime factor
    mov eax, 4
    mov ebx, 1
    mov ecx, factors
    mov edx, 14
    int 0x80

    ; Convert factor to ASCII and print
    add ecx, 13
    mov byte [ecx], al
    mov eax, 4
    mov ebx, 1
    mov edx, 1
    int 0x80

    jmp find_factors

next_factor:
    inc ecx
    jmp find_factors

print_factors:
    ; Print newline
    mov eax, 4
    mov ebx, 1
    mov ecx, newline
    mov edx, 2
    int 0x80

    ; Exit program
    mov eax, 1
    xor ebx, ebx
    int 0x80