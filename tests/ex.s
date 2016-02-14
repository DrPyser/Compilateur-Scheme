	.globl main
main:
	#push-address
	lea if_false0, %rax
	push %rax

	#push-address
	lea if_true0, %rax
	push %rax

	#push-constant
	mov $9, %rax
	push %rax

	jmp scheme_if

if_true0:
	#push-constant
	mov $120, %rax
	push %rax

	#push-constant
	mov $16, %rax
	push %rax

	#push-local
	mov 0(%rsp), %rax
	push %rax

	#push-local
	mov 16(%rsp), %rax
	push %rax

	#mod
	pop %rax
	pop %rbx
	xor %rdx, %rdx
	sar $3, %rbx
	idiv %rbx
	push %rdx

	#println
	mov (%rsp), %rax
	push %rax
	call println

	#xchg
	pop %rax
	xchg %rax, (%rsp)
	push %rax

	#pop
	add $8, %rsp

	#xchg
	pop %rax
	xchg %rax, (%rsp)
	push %rax

	#pop
	add $8, %rsp

	jmp if_end0

if_false0:
	#push-constant
	mov $1, %rax
	push %rax

	#println
	mov (%rsp), %rax
	push %rax
	call println

if_end0:
	pop %rax
	ret

