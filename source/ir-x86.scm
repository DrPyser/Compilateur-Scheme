#! /usr/bin/env gsi

(include "../lib/reader.scm")

(define (parse source-filename)
  (with-input-from-file
      source-filename
    (lambda ()
      (read))))

(define (translate-to-x86 instrs)
  (foldl (lambda (acc instr)
           ;(pp acc)
           ;(pp instr)
           (append acc (compile-instr instr)))
         '()
         instrs))

(define (compile-instr instr)
  (case (vector-ref instr 0)
    ((label) (list (string-append (vector-ref instr 1) ":\n")))
    ((push-local) (map tab (list "#push-local\n" (string-append "mov " (number->string (* 8 (vector-ref instr 1))) "(%rsp), %rax\n") "push %rax\n\n")))
    ((push-constant) (map tab (x86-push-constant (vector-ref instr 1))))
    ((add) (map tab (list "#add\n" "pop %rax\n" "pop %rbx\n" "add %rbx, %rax\n" "push %rax\n\n")))
    ((sub) (map tab (list "#sub\n" "pop %rax\n" "pop %rbx\n" "sub %rbx, %rax\n" "push %rax\n\n")))
    ((mul) (map tab (list "#mul\n" "pop %rax\n" "pop %rbx\n" "sar $3, %rax\n" "imul %rbx, %rax\n" "push %rax\n\n")))
    ((div) (map tab (list "#div\n" "pop %rax\n" "pop %rbx\n" "xor %rdx, %rdx\n" "sar $3, %rbx\n" "idiv %rbx\n" "push %rax\n\n")))
    ((mod) (map tab (list "#mod\n" "pop %rax\n" "pop %rbx\n" "xor %rdx, %rdx\n" "sar $3, %rbx\n" "idiv %rbx\n" "push %rdx\n\n")))
    ((return) (map tab (list "pop %rax\n" "ret\n\n")))
    ((pop) (map tab (list "#pop\n" "add $8, %rsp\n\n")))
    ((println) (map tab (list "#println\n" "mov (%rsp), %rax\n" "push %rax\n" "call println\n\n")))
    ((push-address) (map tab (list "#push-address\n" (string-append "lea " (vector-ref instr 1) ", %rax\n") "push %rax\n\n")))
    ((jump) (map tab (list (string-append "jmp " (vector-ref instr 1) "\n\n"))))
    ((xchg) (map tab (list "#xchg\n" "pop %rax\n" "xchg %rax, (%rsp)\n" "push %rax\n\n")))
    ((if) (map tab (list "jmp scheme_if\n\n")))))

(define (tab str)
  (string-append "\t" str))
                 
(define (x86-push-constant constant)
  (cond ((fixnum? constant) (list "#push-constant\n" (string-append "mov $" (number->string (encode-fixnum constant)) ", %rax\n")
                                  "push %rax\n\n"))
        ((boolean? constant) (list "#push-constant\n" (string-append "mov $" (number->string (encode-boolean constant)) ", %rax\n")
                                   "push %rax\n\n"))))
        
(define (encode-fixnum num)
  (arithmetic-shift num 3))

(define (encode-boolean bool)
  (if bool
      9
      1))


(define (parse-args args)
  (let* ((tokens (string-split-at args #\space))
         (options (filter (lambda (str) (char=? (string-ref str 0) #\-)) tokens))
         (filename (last tokens)))
    (cons options filename)))


(define (main . args)
  (let* ((source-filename (last args))
         (options (filter (lambda (str) (char=? (string-ref str 0) #\-)) (init args)))
         (instrs (parse source-filename))
         (stdout? (member "-std" options)))
                                        ;(pp instrs)
     (let ((x86 (cons "\t.globl main\n" (translate-to-x86 instrs))))
                                        ;(pp x86)
       (call-with-output-file
           (string-append (path-strip-extension source-filename) ".s")
         (lambda (file)
           (for-each (lambda (line) (if stdout? (display line)) (display line file)) x86))))))
