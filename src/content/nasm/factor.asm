section .data
    prompt db "Enter a number (1-1000000): ", 0
    factors db "Prime factors: ", 0
    newline db 10, 0
    buffer db 32 ; buffer to store the converted number

section .bss
    number resd 1

section .text
    global _start

_start:
    ; Display prompt
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt
    mov rdx, 26
    syscall

    ; Read number from user
    mov rax, 0
    mov rdi, 0
    mov rsi, number
    mov rdx, 4
    syscall

    ; Convert number to string
    mov rax, [number]
    mov rdi, buffer
    call itoa

    ; Convert string to number
    mov rdi, buffer
    call atoi

    ; Print prime factors
    mov rax, [number]
    mov rbx, 2
    mov rdx, 0

find_factors:
    cmp rax, rbx
    jle done

    mov rdx, 0
    div rbx
    cmp rdx, 0
    jne next

    ; rbx is a factor, convert it to string
    push rax
    push rbx
    mov rax, rbx
    mov rdi, buffer
    call itoa
    pop rbx
    pop rax

    ; Print the factor
    mov rax, 1
    mov rdi, 1
    mov rsi, factors
    mov rdx, 14
    syscall

    ; continue finding factors
    jmp find_factors

next:
    inc rbx
    jmp find_factors

done:
    ; Print newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall

    ; Exit program
    mov rax, 60
    xor rdi, rdi
    syscall

itoa:
    ; Convert a number in rax to a string in rdi
    xor rcx, rcx
    mov r9, 10

itoa_convert_loop:
    xor rdx, rdx
    div r9
    add dl, '0'
    dec rdi
    mov byte [rdi], dl
    inc rcx
    test rax, rax
    jnz itoa_convert_loop

    ; Null-terminate the string
    dec rdi
    mov byte [rdi], 0

    ; Return the length of the string in rax
    mov rax, rcx
    ret

atoi:
    ; Convert a string in rdi to a number in rax
    xor rax, rax
    xor rbx, rbx
    mov rcx, 10

atoi_convert_loop:
    movzx edx, byte [rdi]
    cmp dl, '0'
    jb atoi_done
    cmp dl, '9'
    ja atoi_done
    sub dl, '0'
    imul rbx, rcx
    add rbx, rax
    mov rax, rbx
    inc rdi
    jmp atoi_convert_loop

atoi_done:
    ret
