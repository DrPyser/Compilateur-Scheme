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
;; #(num-eq)                   pop values a and b from stack and push #t if (= a b), #f otherwise
;; #(gt)                       pop values a and b from stack and push #t if (> a b), #f otherwise
;; #(lt)                       pop values a and b from stack and push #t if (< a b), #f otherwise
;; #(gte)                      pop values a and b from stack and push #t if (>= a b), #f otherwise
;; #(lte)                      pop values a and b from stack and push #t if (<= a b), #f otherwise
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
  (append (list* (vector 'label name)
                 (comp-expr expr 0 '((argc . 1))))
          (list (vector 'return))))

(define-macro (inc var)
  `(set! ,var (+ ,var 1)))

(define (multimap f l . ls)
  (if (null? ls)
      (map f l)
      (if (null? l)
          '()
          (cons (apply f (map car (cons l ls)))
                (apply multimap f (map cdr (cons l ls)))))))

(define (range from to step)
  (cond ((< step 0)
         (if (<= from to)
             '()
             (cons from (range (+ step from) to step))))
        ((> step 0)
         (if (>= from to)
             '()
             (cons from (range (+ step from) to step))))))

(define (mappend f l)
  (apply append (map f l)))

(define comp-expr
  (let ((if-counter 0))
    (lambda (expr fs cte) ;; fs = frame size
      ;; cte = compile time environment
      (cond
       ;; handle this form: 123
       ((or (number? expr) (boolean? expr))
        (vector 'push-constant expr))

       ;; handle this form: var
       ((symbol? expr)
        (let ((x (assoc expr cte)))
          (if x
              (let ((index (cdr x)))
                (vector 'push-local (+ fs index)))
              (error "undefined variable" expr))))

       ;; handle this form: (let ((var expr)) body)
       ((and (list? expr)
             (= (length expr) 3)
             (eq? (list-ref expr 0) 'let))
        (let ((bindings (list-ref expr 1)))
          (append
           (multimap (lambda (i binding)
                       (comp-expr (cadr binding) (+ fs i) cte))
                     (range 0 (length bindings) 1)
                     bindings)
           (comp-expr (list-ref expr 2)
                      (+ (length bindings) fs)
                      (append (multimap (lambda (i binding)
                                          (cons (car binding) (- (+ fs i))))
                                        (range 1 (+ 1 (length bindings)) 1)
                                        bindings)
                              cte))
           (mappend (lambda (x) (list (vector 'xchg) (vector 'pop)))
                    bindings))))
                        
       ;; handle these forms:
       ;;   (+ expr1 expr2)
       ;;   (- expr1 expr2)
       ;;   (* expr1 expr2)
       ;;   (/ expr1 expr2)
       ((and (list? expr)
             (= (length expr) 3)
             (member (list-ref expr 0) '(+ - * / modulo quotient)))
        (list
         (comp-expr (list-ref expr 1) fs cte)
         (comp-expr (list-ref expr 2) (+ fs 1) cte)
         (case (list-ref expr 0)
           ((+) (vector 'add))
           ((-) (vector 'sub))
           ((*) (vector 'mul))
           ((/) (vector 'div))
           ((quotient) (vector 'div))
           ((modulo) (vector 'mod)))))

       ;; handle these forms:
       ;; (= expr1 expr2)
       ;; (< expr1 expr2)
       ;; (<= expr1 expr2)
       ;; (> expr1 expr2)
       ;; (>= expr1 expr2)

       ((and (list? expr)
             (= (length expr) 3)
             (member (list-ref expr 0) '(= < >)))
        (list
         (comp-expr (list-ref expr 2) fs cte)
         (comp-expr (list-ref expr 1) (+ fs 1) cte)
         (case (list-ref expr 0)
           ((=) (vector 'num-eq))
           ((>) (vector 'gt))
           ((<) (vector 'lt))
           ((>=) (vector 'gte))
           ((<=) (vector 'lte)))))

       ;; handle these forms:
       ;;   (println expr)
       ((and (list? expr)
             (= (length expr) 2)
             (member (list-ref expr 0) '(println)))
        (case (car expr)
          ((println) (list
                      (comp-expr (cadr expr) fs cte)
                      (vector 'println)))))


       ;;handle (if expr1 expr2 expr3)
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
          (list
           (vector 'push-address false-label)
           (vector 'push-address true-label)
           (comp-expr (list-ref expr 1) (+ fs 2) cte)
           (vector 'if)
           (vector 'label true-label)
           (comp-expr (list-ref expr 2) (+ fs 3) cte)
           (vector 'jump end-label)
           (vector 'label false-label)
           (comp-expr (list-ref expr 3) (+ fs 4) cte)
           (vector 'label end-label))))
       
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
