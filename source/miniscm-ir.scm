#! /usr/bin/env gsi

;;; File: "miniscm-ir.scm"

;; To compile the file "file.scm" to "file.ir" do:
;;
;;   ./miniscm-ir file.scm
;;
;; Before doing this, do a "chmod +x miniscm-ir.scm".

;;; Source language syntax:

;; <expr> ::=  <number>
;;         |   <var>
;;         |   (let ((<var> <expr>)) <expr>)
;;         |   (+ <expr> <expr>)
;;         |   (- <expr> <expr>)
;;         |   (* <expr> <expr>)
;;         |   (/ <expr> <expr>)
;;         |   (println <expr>)

;;; IR instructions:

;; #(label <string>)           label with name = <string>
;; #(push-constant <number>)   push <number> on stack
;; #(push-local <n>)           push stack slot <n> from top (0 is top of stack)
;; #(add)                      pop values a and b from stack and push a+b
;; #(sub)                      pop values a and b from stack and push a-b
;; #(mul)                      pop values a and b from stack and push a*b
;; #(div)                      pop values a and b from stack and push a/b
;; #(println)                  print value at top of stack
;; #(xchg)                     pop values a and b from stack and push a then b
;; #(pop)                      pop a value from stack and discard it
;; #(jump <string>)            jump to the label <string>
;; #(push-address <string>)    push the address of the label <string> on the stack
;; #(return)                   pop a value from stack (the result) and pop
;;                             the return address and jump to it

(include "../lib/reader.scm")

;;; Parsing

(define (parse source-filename)
  (with-input-from-file
      source-filename
    (lambda ()
      (read))))

;;; Translation of AST to IR


(define (translate-to-ir ast)
  (flatten (comp-function "main" ast)))


(define (comp-function name expr)   
   (comp-expr expr 0 '((argc . 1))
              (lambda (code fs cte)
                (append (list* (vector 'label name)
                               code)            
                        (list (vector 'return))))))


(define-macro (inc var)
  `(set! ,var (+ ,var 1)))

(define comp-expr
  (let ((if-counter 0))
    (lambda (expr fs cte k) ;; fs = frame size
      ;; cte = compile time environment
      ;; k = continuation
      (cond
       ;; handle this form: 123
       ((or (number? expr) (boolean? expr))
        (k (list (vector 'push-constant expr)) fs cte))

       ;; handle this form: var
       ((symbol? expr)
        (let ((x (assoc expr cte)))
          (if x
              (let ((index (cdr x)))
                (k (list (vector 'push-local (+ fs index))) fs cte))
              (error "undefined variable" expr))))

       ;; handle this form: (let ((var expr)) body)
       ((and (list? expr)
             (= (length expr) 3)
             (eq? (list-ref expr 0) 'let))
        (let ((binding (list-ref (list-ref expr 1) 0)))          
           (comp-expr (list-ref binding 1) fs cte
                      (lambda (code1 fs cte)
                        (comp-expr (list-ref expr 2)
                                   (+ fs 1)                                   
                                   (cons (cons (list-ref binding 0)
                                               (- (+ fs 1)))
                                         cte)
                                   (lambda (code2 fs cte)
                                     (k (append code1
                                                code2
                                                (list (vector 'xchg)
                                                      (vector 'pop)))
                                        fs
                                        cte)))))))
                        
       ;; handle these forms:
       ;;   (+ expr1 expr2)
       ;;   (- expr1 expr2)
       ;;   (* expr1 expr2)
       ;;   (quotient expr1 expr2)
       ;;   (modulo expr1 expr2)
       ((and (list? expr)
             (= (length expr) 3)
             (member (list-ref expr 0) '(+ - * / quotient modulo)))
        (comp-expr (list-ref expr 2) fs cte
                   (lambda (code1 fs cte)
                     (comp-expr (list-ref expr 1) (+ fs 1) cte
                                (lambda (code2 fs cte)
                                  (k (append code1 ;expr1
                                             code2 ;expr2
                                             (list (case (list-ref expr 0)
                                                     ((+) (vector 'add))
                                                     ((-) (vector 'sub))
                                                     ((*) (vector 'mul))
                                                     ((quotient /) (vector 'div))
                                                     ((modulo) (vector 'mod)))))
                                     fs
                                     cte))))))

       ;; handle these forms:
       ;;   (println expr)
       ((and (list? expr)
             (= (length expr) 2)
             (member (list-ref expr 0) '(println)))
        (comp-expr (list-ref expr 1) fs cte
                   (lambda (code fs cte)
                     (k (append code ;expr
                                (list (case (list-ref expr 0)
                                        ((println) (vector 'println)))))
                        fs cte))))
                   
       ;;handle (if test expr1 expr2)
       ((and (list? expr)
             (= (length expr) 4)
             (eq? (car expr) 'if))
        (let ((test (cadr expr))
              (true-expr (list-ref expr 2))
              (false-expr (list-ref expr 3))
              (true-label (string-append "if_true" (number->string if-counter)))
              (false-label (string-append "if_false" (number->string if-counter)))
              (end-label (string-append "if_end" (number->string if-counter))))
          (inc if-counter)
          (comp-expr test (+ fs 2) cte
                     (lambda (code1 fs cte)                      
                       (comp-expr true-expr fs cte
                                  (lambda (code2 fs cte)                                    
                                    (comp-expr false-expr fs cte
                                               (lambda (code3 fs cte)
                                                 (k (append (list* (vector 'push-address false-label)
                                                                   (vector 'push-address true-label)
                                                                   code1) ;code for "test". 
                                                            (list* (vector 'if)
                                                                   (vector 'label true-label)
                                                                   code2) ;code for "true-expr"
                                                            (list* (vector 'jump end-label) ;Skip the "false" block
                                                                   (vector 'label false-label)
                                                                   code3) ;code for "false-expr"
                                                            (list (vector 'label end-label)))
                                                    fs cte)))))))))
                
       (else
        (begin (display expr)
               (error "comp-expr cannot handle expression")))))))

;; Utility:

(define (flatten lst)
  (append-flatten lst '()))

(define (append-flatten lst rest)
  (cond ((null? lst)
         rest)
        ((pair? lst)
         (append-flatten (car lst)
                         (append-flatten (cdr lst)
                                         rest)))
        (else
         (cons lst rest))))

(define (list* . forms)
  (if (null? (cdr forms))
      (car forms)
      (cons (car forms) (apply list* (cdr forms)))))

;; Main program:

(define (main source-filename)
  (let ((ast (parse source-filename)))
    (let ((ir (translate-to-ir ast)))
      (with-output-to-file
          (string-append (path-strip-extension source-filename) ".ir")
        (lambda ()
          (pretty-print ir))))))
