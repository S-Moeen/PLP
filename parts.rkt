#lang racket
(provide (all-defined-out))
(require racket/set)
(define null 'null)
(define setify (lambda (x) (list->set (cons x '())) ))
(define listify (lambda (x) (list->set (cons x '())) ))
(define get_firsts (lambda (x) (if (equal? x null) null (car x)) ) )
(define get_seconds (lambda (x) (if (equal? x null) null (cadr x)) ) )
(define check_validators (lambda (state validators) (foldl (lambda (validator prev) (and prev (validator state)) ) #t validators)))
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

(define (get_simple_dest state)
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

;(define (get_join_node state validator1 validator2 other_validators)
;  (lambda
;      ()
;   (let (
;       [s1 (map get_firsts state)]
;       [s2 (map get_seconds state)]
;       )
;(if (and (check_validators state other_validators)
;           (validator1 s1)
;           (validator2 s2)           
;           )
;  state
;  #f)
;    )
;   )
; )


;(define (get_merger_node state validator1 validator2 other_validators)
;  (lambda
;      ()
;    ;if both are null 
;  (if (and (check_validators state other_validators)
;           (validator1 state)
;           (validator2 state)           
;           )
;  state
;  #f)
;   )
; )

;**************************************************************************************
;**************************************************************************************
;**************************************************************************************
;                                    Channels
;**************************************************************************************
;**************************************************************************************
;**************************************************************************************

(define (get_sync input)
  ;it gets its input and output in a list as nodes
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
  ;it gets its input and output in a list as nodes
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
;                                    Block
;**************************************************************************************
;**************************************************************************************
;**************************************************************************************

(define get_merger_block
  (lambda
      (state source_node1 source_node2 validators)
    (get_dest state (cons (get_merger_channel source_node1 source_node2) validators) )
    )
  )


(define get_join_block
  (lambda
      (state source_node1 source_node2 validators)
    (get_dest state (cons (get_join_channel source_node1 source_node2) validators))
    )
  )

(define get_replicator_block
  (lambda
      (state1 state2 source_node validators)
    (cons (get_dest state1 (cons (get_sync source_node) validators)) (get_dest state2 (cons (get_sync source_node) validators)))
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


(define simple_dest1 (get_simple_dest (list 1 2 null 4 5) ))
(define simple_dest2 (get_simple_dest (list 1 2 3 4 5) ))
(define simple_dest3 (get_simple_source (list 1 6 3 4 null) ))
(define simple_dest4 (get_simple_source (list 9 3 3 4 null) ))
(define simple_dest5 (get_simple_source (list '(1 9) '(6 3) '(3 3) '(4 4) null) ))


(define valid_sync (get_sync (list simple_source1 simple_dest1 )))
(define invalid_sync (get_sync (list simple_source1 simple_dest2 )))
(define valid_sync_drain (get_sync_drain simple_source3 simple_dest4 ))
;(define invalid_sync_drain (get_sync_drain (list simple_source1 simple_dest3 )))

;(define sync (get_sync simple_source1))
;(define dest (get_dest (list 1 2 null 4 5) sync))



;(define a (get_simple_source (list 1 null 100 -10 0)))
;(define core (get_replicator_node a))
;(define sync (get_sync a))
;(define sync_drain (get_sync_drain a))
;(define b (get_dest (list 1 null -200 -15 20) (listify sync_drain)))
;(define c (get_dest (list 1 null 100 -10 0) (listify sync)))

;(phase1 (list 1 null 100 -10 0) (list 1 null -200 -15 20) (list 1 null 100 -10 0) )

;(define a (get_simple_source (list 1 null null -10 null)))
;(define fif (get_fifo1 a))
;(define b (get_dest (list null null 1 null -10) (listify fif)))

;(define a (get_simple_source (list 1 null null -10 null)))
;(define b (get_simple_source (list null 2 3 null 12)))
;(define c (get_merger_block (list 1 2 4 -10 12) a b '()))

;(define c (get_dest (list '(1 9) '(6 3) '(3 3) '(4 4) null) (list (get_join_channel simple_source3 simple_source4))))


;validjoin
;(define c (get_dest (list '(1 9) '(6 3) '(3 3) '(4 4) null) (list (get_join_channel simple_source3 simple_source4 )) ) )
;invalid join
;(define c (get_dest (list '(1 9) '(3 3) '(3 3) '(4 4) null) (list (get_join_channel simple_source3 simple_source4 )) ) )

;valid sync
;(define c (get_dest (list 1 2 3 4 5) (list (get_sync simple_source2)) ) )
;invalid sync
; (define c (get_dest (list 1 2 3 4 3) (list (get_sync simple_source2)) ) )

;valid sync_drain
;(define c (get_dest (list 1 6 3 4 null) (list (get_sync_drain simple_source4 (set (list 1 6 3 4 null))) ) ))
;invalid sync_drain
;(define c (get_dest (list 1 6 null 4 null) (list (get_sync_drain simple_source4 (set (list 1 6 3 4 null))) ) ))

;valid merger
;(define c (get_dest (list 1 2 3 4 5) (list (get_merger_channel simple_source1 simple_source2  ) )))
;invalid merger
;(define c (get_dest (list 3 2 3 4 5) (list (get_merger_channel simple_source1 simple_source2  ) )))

;valid fifo1
;(define c (get_dest (list null null null 3 null) (list (get_fifo1 simple_source5) )))
;(define c (get_dest (list null null null 4 null) (list (get_fifo1 simple_source5) )))
