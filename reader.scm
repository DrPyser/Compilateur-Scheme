;; File: read-simple.scm

(define readtable (make-table))

(define (set-reader-macro c f . t)
  (if (null? t)
      (table-set! readtable c f)
      (table-set! (car t) c f)))

(define (get-reader-macro c . t)
  (if (null? t)
      (table-ref readtable c #f)
      (table-ref (car t) c #f)))

(define (set-delimiter c . t)
  (if (null? t)
      (table-set! readtable c (lambda (c) (error "This character cannot be read alone")))
      (table-set! (car t) c (lambda (c) (error "This character cannot be read alone")))))

(define (read)
  (let* ((c (peek-char-non-whitespace))
         (readermacro (get-reader-macro c)))
    (cond ((eof-object? c)
           c)
          ((char=? #\. c) (error "Syntax error: wrong context for dot"))
          (readermacro (begin (read-char) (readermacro c)))
          ((char=? c #\()
           (read-char) ;; skip "("
            (read-list))
          ((char=? c #\#)
           (read-char)
           (if (char=? (peek-char) #\\)
               (begin (read-char) (list->char (read-literal-char)))                              
               (let ((form (read)))
                 (cond ((symbol? form)
                        (case (read) 
                          ((t) #t)
                          ((f) #f)))
                       ((list? form)
                        (apply vector form))))))
          ((char=? c #\")
           (read-char)
           (list->string (read-string)))
          ((char=? c #\')
           (read-char)
           `(quote ,(read)))
          (else
           (read-char) ;; skip first char
           (let ((s (list->string (cons c (read-symbol)))))
             (or (string->number s)
                 (string->symbol s)))))))

(define (read-list)
  (let ((c (peek-char-non-whitespace)))
    (cond ((char=? c #\)) (begin
                            (read-char) ;; skip ")"
                            '()))
          ((char=? c #\.) (begin (read-char) ;; skip "."
                                 (let ((rest (read)))
                                   (if (char=? (peek-char-non-whitespace) #\))
                                       (begin (read-char) rest)
                                       (error "Syntax error: too many values after dot")))))
          (else (let ((first (read)))
                  (let ((rest (read-list)))
                    (cons first rest)))))))

(define (read-delimited-list delimiter)
  (let ((c (peek-char-non-whitespace)))
    (cond ((char=? c delimiter) (begin
                                  (read-char) ;; skip delimiter
                                  '()))
          ((char=? c #\.) (begin (read-char) ;; skip "."
                                 (let ((rest (read)))
                                   (if (char=? (peek-char-non-whitespace) delimiter)
                                       (begin (read-char) ;;skip delimiter
                                              rest)
                                       (error "Syntax error: too many values after dot")))))
          (else (let ((first (read)))
                  (let ((rest (read-delimited-list delimiter)))
                    (cons first rest)))))))

(define (read-symbol)
  (let ((c (peek-char)))
    (if (or (eof-object? c)
            (char=? c #\()
            (char=? c #\))
            (char<=? c #\space)
            (get-reader-macro c))
        '()
        (begin
          (read-char)
          (cons c (read-symbol))))))

(define (read-string)
  (let ((c (peek-char)))
    (cond ((eof-object? c) (error "Invalid syntax: non-terminated string"))
          ((char=? c #\") (begin (read-char) '()))
          ((char=? c #\\) (begin (read-char)
                                 (let ((ch (read-char)))
                                   (case ch
                                     ((n) (cons #\newline (read-string)))
                                     (else (cons ch (read-string)))))))
          (else (begin
                  (read-char)
                  (cons c (read-string)))))))


(define (list->char lst)
  (if (= 1 (length lst))
      (car lst)
      (let ((str (list->string lst)))
        (cond ((string=? str "newline") #\newline)
              ((string=? str "space") #\space)
              ((string=? str "backspace") #\backspace)
              ((string=? str "nul") #\nul)
              ((string=? str "return") #\return)
              ((string=? str "page") #\page)
              ((string=? str "tab") #\tab)))))
                
(define (read-literal-char)
  (let ((fst (read-char)))
    (if (char-whitespace? (peek-char))
        (list fst)
        (cons fst (read-literal-char)))))

(define (peek-char-non-whitespace)
  (let ((c (peek-char)))
    (if (or (eof-object? c)
            (char>? c #\space))
        c
        (begin
          (read-char)
          (peek-char-non-whitespace)))))

;; try it...

(set-reader-macro #\[ (lambda (c) (read-delimited-list #\])))

(set-reader-macro #\] (lambda (s c) (error "This character isn't meant to be read alone!")))

(pp (read))


