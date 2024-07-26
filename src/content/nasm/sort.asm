section .data
    array db 10 dup(0)  ; Array to store the random integers

section .text
    global _start

_start:
    ; Generate random numbers and store them in the array
    mov ecx, 10  ; Number of elements in the array
    mov esi, array
generate_numbers:
    ; Generate a random number between 0 and 99
    mov eax, 0x2d  ; System call number for SYS_getrandom
    mov edi, esi
    mov edx, 1  ; Number of bytes to generate
    xor ebx, ebx  ; Flags (set to 0)
    int 0x80  ; Call the kernel

    ; Scale the random number to be between 0 and 9
    xor edx, edx
    mov dl, byte [esi]
    mov byte [esi], dl

    ; Print the unsorted array
    mov eax, 4  ; System call number for SYS_write
    mov ebx, 1  ; File descriptor for stdout
    mov ecx, esi
    mov edx, 10  ; Number of bytes to write
    int 0x80  ; Call the kernel

    ; Sort the array using bubble sort
    mov ecx, 10  ; Number of elements in the array
sort_array:
    xor edx, edx
    mov esi, array
inner_loop:
    mov al, byte [esi]
    cmp al, byte [esi + 1]
    jle skip_swap
    xchg al, byte [esi + 1]
    mov byte [esi], al
skip_swap:
    inc esi
    loop inner_loop

    ; Print the sorted array
    mov eax, 4  ; System call number for SYS_write
    mov ebx, 1  ; File descriptor for stdout
    mov ecx, array
    mov edx, 10  ; Number of bytes to write
    int 0x80  ; Call the kernel

    ; Exit the program
    mov eax, 1  ; System call number for SYS_exit
    xor ebx, ebx  ; Exit status (set to 0)
    int 0x80  ; Call the kernel

    