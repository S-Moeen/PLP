#lang racket
(provide (all-defined-out))

(require 2htdp/batch-io)
(define null 'null)
(define a (read-file "SEC1_DATA.txt"))
(define b (read-lines "SEC1_DATA.txt"))
(define (my_trim lst) (map (lambda (x) (string-replace (string-replace (string-replace x " " "") "(" "") ")" "")) lst))
(define (make_dict lst) (map (lambda (x) (cdr (regexp-split #rx":|," x))) lst) )
(define (cast lst) (map (lambda (x) (map (lambda (y) (or
                                                      (string->number y)
                                                      (if (equal? y "null")
                                                          null
                                                          y
                                                       )
                                                      )) x) ) lst) )
(define (parse text) (cast (make_dict (my_trim (filter (lambda (x) (not (equal? x "")) ) text)))))
(define
  (convert->parts input size output)
  (if (null? input)
      output
      (convert->parts (cdddr input) size (append output (cons (list (car input) (cadr input) (caddr input ) ) '())))
   )
  )

(define (read_parse address size)  (convert->parts (parse (read-lines address)) size '()))