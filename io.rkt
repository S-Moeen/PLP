#lang racket
(provide (all-defined-out))
(require 2htdp/batch-io)
(define null 'null)

(define (my_trim lst) (map (lambda (x) (string-replace (string-replace (string-replace x " " "") "(" "") ")" "")) lst))
;removes "(",")" and " "

(define (make_dict lst) (map (lambda (x) (cdr (regexp-split #rx":|," x))) lst) )
;splits each list and removes what is behind ":"

(define (cast lst) (map (lambda (x) (map (lambda (y) (or
                                                      (string->number y)
                                                      (if (equal? y "null")
                                                          null
                                                          y
                                                       )
                                                      )) x) ) lst) )
;Casts each element of input

(define (parse text) (cast (make_dict (my_trim (filter (lambda (x) (not (equal? x "")) ) text)))))
;parses input to lists

(define
  (convert->parts input size output)
  (if (null? input)
      output
      (convert->parts (cdddr input) size (append output (cons (list (car input) (cadr input) (caddr input ) ) '())))
   )
  )
;packages inputs of simulations

(define (read_parse address size)  (convert->parts (parse (read-lines address)) size '()))
;reads from address and packages with size


(define (write_output lst address) (write-file address (string-append* ""  (map (lambda (x) (if x "True\n" "False\n" ) ) lst))))
;writes boolean list to file