#lang racket
(provide (all-defined-out))
(require racket/set)
(define null 'null)
(define setify (lambda (x) (list->set (cons x '())) ))
(define listify (lambda (x) (list->set (cons x '())) ))
(define check_nodes (lambda (nodes) (foldl (lambda (node prev) (and (not (equal? (node) (set))) #t) ) #t nodes)))
(define
  (multiple_intersect lst)
  (cond
    ([equal? (length lst) 1] (car lst))
    ([null? lst] (set))
    (else (set-intersect (car lst ) (multiple_intersect (cdr lst)) ))
    )
  )
(define (cartesian_product l1 l2)
  (foldl (lambda (x acc) (append acc (cartesian_product_helper x l2) )) '() l1
         )
  )
(define (cartesian_product_helper element l2)
  (foldl (lambda (x acc) (append (cons (list element x) '()) acc)) '() l2
         )
  )

;**************************************************************************************
;**************************************************************************************
;**************************************************************************************
;                                    NODES
;**************************************************************************************
;**************************************************************************************
;**************************************************************************************
(define (get_simple_source state)
  ;it gets a list as a state for node
  (lambda
      ()
    (setify state)
    )
  )


(define (get_dest state validators)
  ;it gets a list as a state for node
  (lambda
      ()
    (if
     (set-member? (multiple_intersect (map (lambda (x) (x)) validators)) state)
     (set state)
     (set)
     )
    )
  )

(define (get_middle_node validators)
  ;it gets a list as a state for node
  (lambda
      ()
    (multiple_intersect (map (lambda (x) (x)) validators))
    )
  )


(define (get_replicator_node source)
  source
  )

;**************************************************************************************
;**************************************************************************************
;**************************************************************************************
;                                    Channels
;**************************************************************************************
;**************************************************************************************
;**************************************************************************************

(define (get_sync input)
  (lambda
      ()
    (input)
    )
  )

(define (_get_help_sync_drain s1 s2)
  (foldl (lambda (x y acc) (and acc (or (and (equal? null x) (equal? null y)) (and (not (equal? null x)) (not (equal? null y)))) ))
         #t
         s1
         s2
         )
  )
(define (get_sync_drain input1 states)
  (lambda
      ()
    (list->set (foldl
                (lambda
                    (x acc)
                  (if (_get_help_sync_drain (car x) (cadr x))
                      (cons (cadr x) acc)
                      acc
                      )
                  )
                (list)
                (cartesian_product (set->list  (input1)) (set->list states))
                ))
    )
  )

(define (check_fifo1_full buffer state)
  (if (null? state)
      (list (list))
      (append (map (lambda (x) (cons buffer x) ) (check_fifo1_empty (cdr state)) )
              (map (lambda (x) (cons null x) ) (check_fifo1_full buffer (cdr state)) )
              )
      )
  )

(define (check_fifo1_empty state)
  (if (null? state)
      (list (list))
      (if (equal? null (car state))
          (map (lambda (x) (cons null x) ) (check_fifo1_empty (cdr state)) )
          (map (lambda (x) (cons null x) ) (check_fifo1_full (car state) (cdr state)) )
          )
      )
  )


(define
  (get_fifo1 input)
  (lambda() (list->set (foldl (lambda (x acc) (append x acc)) '() (map check_fifo1_empty (set->list (input))))))
  )

  
(define (_merger_helper input1 input2)
  (if (null? input1)
      '()
      (cond
        [(and (equal? (car input1) null) (equal? (car input2) null) ) (cons null (_merger_helper (cdr input1) (cdr input2)))]
        [(and (not (equal? (car input1) null)) (equal? (car input2) null) ) (cons (car input1) (_merger_helper (cdr input1) (cdr input2)))]
        [(and (equal? (car input1) null) (not (equal? (car input2) null) )) (cons (car input2) (_merger_helper (cdr input1) (cdr input2)))]
        [else (cons (car input1) (_merger_helper (cdr input1) (cdr input2)))]
        )
      )
  )

(define (get_merger_channel input1 input2 )
  (lambda
      ()
    (list->set
     (map
      (lambda
          (x)
        (_merger_helper (car x) (cadr x))
        )
      (cartesian_product (set->list (input1)) (set->list (input2)))
      )
     )
    )
  )

(define (get_join_channel input1 input2 )
  (lambda
      ()
    (list->set
     (map
      (lambda
          (x)
        (foldl (lambda (l r acc) (append (if (and (equal? l null) (equal? r null)) (list null) (cons (list l r) '()) ) acc))
               '() (reverse (car x))  (reverse (cadr x)))
        )
      (cartesian_product (set->list (input1)) (set->list (input2)))
      )
     )
    )
  )
  


  


;**************************************************************************************
;**************************************************************************************
;**************************************************************************************
;                                    data
;**************************************************************************************
;**************************************************************************************
;**************************************************************************************



(define simple_source1 (get_simple_source (list 1 2 null 4 5) ))
(define simple_source2 (get_simple_source (list 1 2 3 4 5) ))
(define simple_source3 (get_simple_source (list 1 6 3 4 null) ))
(define simple_source4 (get_simple_source (list 9 3 3 4 null) ))
(define simple_source7 (get_simple_source (list 9 null 3 4 null) ))
(define simple_source5 (get_simple_source (list null 3 4 null null) ))
(define simple_source6 (get_simple_source (list null null null 3 null) ))



