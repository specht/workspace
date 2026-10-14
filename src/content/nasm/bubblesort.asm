; Bubblesort in Assembler (x86-64, Linux): zieht 10 Zufallszahlen von 0 bis
; 99, gibt sie aus, sortiert sie und gibt sie noch einmal aus.

section .data
    original db "Original array: "
    original_len equ $ - original
    sorted db "Sorted array: "
    sorted_len equ $ - sorted
    space db " "
    newline db 0x0a

section .bss
    array resb 10           ; die 10 Zahlen, je ein Byte
    digits resb 4           ; Platz für die Ziffern einer Zahl

section .text
    global _start

_start:
    ; 10 Zufallsbytes vom Kernel holen (Systemaufruf getrandom)
    mov rax, 318
    mov rdi, array
    mov rsi, 10
    xor rdx, rdx
    syscall

    ; jedes Byte auf 0 bis 99 bringen: der Rest beim Teilen durch 100
    xor rcx, rcx
scale:
    movzx eax, byte [array + rcx]
    xor edx, edx
    mov ebx, 100
    div ebx
    mov byte [array + rcx], dl
    inc rcx
    cmp rcx, 10
    jl scale

    ; unsortiert ausgeben
    mov rsi, original
    mov rdx, original_len
    call print_text
    call print_array

    ; Bubblesort: Nachbarn vertauschen, die falsch herum stehen, bis ein
    ; Durchlauf nichts mehr vertauscht
    mov r8, 9               ; Vergleiche in diesem Durchlauf
outer:
    xor rcx, rcx            ; Position im Array
    xor r9, r9              ; 1, sobald etwas vertauscht wurde
inner:
    mov al, byte [array + rcx]
    mov bl, byte [array + rcx + 1]
    cmp al, bl
    jbe no_swap
    mov byte [array + rcx], bl
    mov byte [array + rcx + 1], al
    mov r9, 1
no_swap:
    inc rcx
    cmp rcx, r8
    jl inner
    test r9, r9
    jz done                 ; nichts vertauscht: fertig
    dec r8
    jnz outer

done:
    ; sortiert ausgeben
    mov rsi, sorted
    mov rdx, sorted_len
    call print_text
    call print_array

    ; Programm beenden
    mov rax, 60
    xor rdi, rdi
    syscall

; Text ausgeben: rsi zeigt auf den Text, rdx ist seine Länge
print_text:
    mov rax, 1
    mov rdi, 1
    syscall
    ret

; die 10 Zahlen mit Leerzeichen dazwischen, danach ein Zeilenumbruch
print_array:
    xor r12, r12
print_next:
    movzx eax, byte [array + r12]
    call print_number
    inc r12
    cmp r12, 10
    je print_end
    mov rsi, space
    mov rdx, 1
    call print_text
    jmp print_next
print_end:
    mov rsi, newline
    mov rdx, 1
    call print_text
    ret

; die Zahl in eax als Ziffern ausgeben, die Ziffern von hinten nach vorne
print_number:
    mov rdi, digits + 4
    mov ebx, 10
next_digit:
    xor edx, edx
    div ebx
    add dl, '0'
    dec rdi
    mov byte [rdi], dl
    test eax, eax
    jnz next_digit
    mov rsi, rdi
    mov rdx, digits + 4
    sub rdx, rdi
    call print_text
    ret
