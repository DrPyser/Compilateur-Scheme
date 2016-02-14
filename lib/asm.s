    # A few useful functions for printing Scheme data
    # Uses stdio.s

    .text
    
    .globl println
    
println:
    push    %rax
    push    %rcx

    mov     3*8(%rsp), %rax #the value to print
    push    %rax #put it back on top
    mov     $7, %rcx
    and     %rcx, %rax #get the 3 most significant bit
    cmp     $0, %rax # 000 = fixnum
    je      print_ln_fixnum
    cmp     $1, %rax # 001 = boolean
    je      print_ln_boolean

print_ln_fixnum:
    call    print_fixnum    
    jmp     print_ln_exit   

print_ln_boolean:
    call    print_boolean
    
print_ln_exit:
    call    print_newline
    pop     %rcx
    pop     %rax
    ret     $8

    .globl print_boolean    
print_boolean:
    push    %rax
    
    mov     2*8(%rsp), %rax
    shr     $3, %rax
    cmp     $0, %rax
    je      print_boolean_false
    jmp     print_boolean_true    

print_boolean_true:
    lea     true(%rip), %rax
    push    %rax
    call    print_string
    jmp      print_boolean_exit
    
print_boolean_false:
    lea     false(%rip), %rax
    push    %rax
    call    print_string

print_boolean_exit:
    pop     %rax #restore rax
    ret     $8

    .globl print_fixnum
print_fixnum:
    push    %rax

    mov     2*8(%rsp), %rax
    sar     $3, %rax
    push    %rax
    call    print_word_dec

    pop     %rax
    ret     $8

    .globl print_newline
print_newline:
    push    %rax
    
    lea     nl(%rip), %rax
    push    %rax
    call    print_string

    pop     %rax
    
    ret

    
    .globl scheme_if    
scheme_if:
    pop     %rax
    cmp     $1, %rax # if false
    je      if_false
    jmp     if_true

if_false:
    add     $8, %rsp #discarding the address of the "true" block
    ret

if_true:
    ret     $8 #jumping to the "true" block and discarding the address of the "false" block
    
    
    .globl scheme_gt
scheme_gt:
    push    %rax
    push    %rbx
    
    mov     3*8(%rsp), %rbx
    mov     4*8(%rsp), %rax
    cmp     %rax, %rbx
    jg      scheme_gt_true
    jmp     scheme_gt_false

scheme_gt_true:
    pop     %rbx
    pop     %rax    
    
    movq    $9, 2*8(%rsp) # Move #t at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#t) on the stack

scheme_gt_false:
    pop     %rbx
    pop     %rax

    movq    $1, 2*8(%rsp) # Move #f at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#f) on the stack

    .globl scheme_lt
scheme_lt:
    push    %rax
    push    %rbx
    
    mov     3*8(%rsp), %rbx
    mov     4*8(%rsp), %rax
    cmp     %rax, %rbx
    jl      scheme_lt_true
    jmp     scheme_lt_false

scheme_lt_true:
    pop     %rbx
    pop     %rax    
    
    movq    $9, 2*8(%rsp) # Move #t at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#t) on the stack

scheme_lt_false:
    pop     %rbx
    pop     %rax

    movq    $1, 2*8(%rsp) # Move #f at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#f) on the stack

        .globl scheme_lte
scheme_lte:
    push    %rax
    push    %rbx
    
    mov     3*8(%rsp), %rbx
    mov     4*8(%rsp), %rax
    cmp     %rax, %rbx
    jle     scheme_lte_true
    jmp     scheme_lte_false

scheme_lte_true:
    pop     %rbx
    pop     %rax    
    
    movq    $9, 2*8(%rsp) # Move #t at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#t) on the stack

scheme_lte_false:
    pop     %rbx
    pop     %rax

    movq    $1, 2*8(%rsp) # Move #f at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#f) on the stack

        .globl scheme_gte
scheme_gte:
    push    %rax
    push    %rbx
    
    mov     3*8(%rsp), %rbx
    mov     4*8(%rsp), %rax
    cmp     %rax, %rbx
    jge     scheme_gte_true
    jmp     scheme_gte_false

scheme_gte_true:
    pop     %rbx
    pop     %rax    
    
    movq    $9, 2*8(%rsp) # Move #t at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#t) on the stack

scheme_gte_false:
    pop     %rbx
    pop     %rax

    movq    $1, 2*8(%rsp) # Move #f at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#f) on the stack


        .globl scheme_num_eq
scheme_num_eq:
    push    %rax
    push    %rbx
    
    mov     3*8(%rsp), %rbx
    mov     4*8(%rsp), %rax
    cmp     %rax, %rbx
    je      scheme_num_eq_true
    jmp     scheme_num_eq_false

scheme_num_eq_true:
    pop     %rbx
    pop     %rax    
    
    movq    $9, 2*8(%rsp) # Move #t at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#t) on the stack

scheme_num_eq_false:
    pop     %rbx
    pop     %rax

    movq    $1, 2*8(%rsp) # Move #f at the position of the first argument
    ret     $8 #return and discard only the second argument, leaving the result (#f) on the stack
    
    .data
nl: .asciz "\n"
true:   .asciz "#t"
false:  .asciz "#f"
