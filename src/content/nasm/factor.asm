section .data
    prompt db "Enter a number: ", 0
    answer db "Prime factors: ", 0
    result db "Doubled number: %d", 0
    newline db 0x0a
    space db 0x20

section .bss
    input resb 16

section .text
    global _start

_start:
    ; Prompt the user for input
    mov rax, 1
    mov rdi, 1
    mov rsi, prompt
    mov rdx, 16
    syscall

    ; Read user input
    mov rax, 0
    mov rdi, 0
    mov rsi, input
    mov rdx, 10
    syscall

    ; Write answer
    mov rax, 1
    mov rdi, 1
    mov rsi, answer
    mov rdx, 15
    syscall

    ; Convert input to integer
    mov rdi, input
    call atoi

    mov rdi, 2          ; Initialize divisor to 2
    mov rsi, rax        ; Copy value from rax to rsi

find_factors:
    mov rdx, 0          ; Clear rdx register
    mov rax, rsi        ; Move value in rsi to rax

    div rdi             ; Divide rsi by rdi
    cmp rdx, 0          ; Check if remainder is zero
    jne failed          ; If remainder is not zero, go to next factor

    mov rsi, rax

    push rax
    push rdi
    push rsi
    push rdx

    mov rax, rdi        ; Move divisor to rax

    ; Convert the result to string
    mov byte [input+15], 0
    mov rdi, input+15
    call itoa

    ; Print the result
    mov rax, 1
    mov rsi, rdi
    mov rdi, 1
    mov rdx, input
    add rdx, 15
    sub rdx, rsi
    syscall

    ; Print a space
    mov rax, 1
    mov rdi, 1
    mov rsi, space
    mov rdx, 1
    syscall

    pop rdx
    pop rsi
    pop rdi
    pop rax

    mov rsi, rax
    jmp next_factor
failed:
    mov rax, rsi
    inc rdi             ; Increment divisor
    cmp rdi, rsi        ; Compare divisor with value in rsi
    jle find_factors    ; If divisor is less than or equal to value, continue finding factors

next_factor:
    cmp rdi, rsi        ; Compare divisor with value in rsi
    jle find_factors

    ; Print a newline
    mov rax, 1
    mov rdi, 1
    mov rsi, newline
    mov rdx, 1
    syscall

    ; Exit the program
    mov rax, 60
    xor rdi, rdi
    syscall

atoi:
    xor rax, rax
    mov rcx, 10

atoi_loop:
    xor rbx, rbx
    mov bl, byte [rdi]
    inc rdi
    cmp bl, 0
    je atoi_done
    cmp bl, 10
    je atoi_done

    sub bl, '0'
    mul rcx
    add rax, rbx

    jmp atoi_loop

atoi_done:
    ret

; itoa implementation
itoa:
    xor rcx, rcx
    mov rsi, 10

itoa_loop:
    xor rdx, rdx
    div rsi
    add dl, '0'
    dec rdi
    mov byte [rdi], dl
    test rax, rax
    jnz itoa_loop

itoa_done:
    ret