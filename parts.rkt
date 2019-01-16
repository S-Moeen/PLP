#lang racket
(define null 'null)
(define listify (lambda (x) (cons x '()) ))
(define get_firsts (lambda (x) (if (equal? x null) null (car x)) ) )
(define get_seconds (lambda (x) (if (equal? x null) null (cadr x)) ) )
(define check_validators (lambda (state validators) (foldl (lambda (validator prev) (and prev (validator state)) ) #t validators)))
(define check_nodes (lambda (nodes) (foldl (lambda (node prev) (and prev (node)) ) #t nodes)))
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
     state
  )
  )

(define (get_simple_dest state)
  ;it gets a list as a state for node
  (lambda
    ()
     state
  )
  )

(define (get_dest state validators)
  ;it gets a list as a state for node
  (lambda
    ()
     (check_validators state validators)
  )
  )

(define (get_replicator_node source)
  source
 )

(define (get_join_node state validator1 validator2 other_validators)
  (lambda
      ()
   (let (
       [s1 (map get_firsts state)]
       [s2 (map get_seconds state)]
       )
  (if (and (check_validators state other_validators)
           (validator1 s1)
           (validator2 s2)           
           )
  state
  #f)
    )
   )
 )


(define (get_merger_node state validator1 validator2 other_validators)
  (lambda
      ()
    ;if both are null 
  (if (and (check_validators state other_validators)
           (validator1 state)
           (validator2 state)           
           )
  state
  #f)
   )
 )

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
    (out)
    (
     let
        (
         [in (input)]
         )
      (if (foldl (lambda (a b prev) (and prev (equal? a b)) ) #t in out)
          out
          #f
     )
  )
  )
  )

(define (get_sync_drain input)
  ;it gets its input and output in a list as nodes
  (lambda
    (out)
    (
     let
        (
         [in (input)]
         )
      (if (foldl (lambda (a b prev)
                   (and prev
                        (not
                         (xor (equal? null a) (equal? null b) ))
                        )
                   )
                 #t
                 in
                 out)
          out
          #f
     )
  )
  )
)

(define (check_fifo1_full buffer state1 state2)
  (if (null? state1)
      #t

   (let
      (
       [input (car state1)]
       [output (car state2)]
       [next_inputs (cdr state1)]
       [next_outputs (cdr state2)]
       )
    (cond
      [(and (equal? input null) (equal? output null))
       (check_fifo1_full buffer next_inputs next_outputs)]
      
      [(and (not (equal? input null)) (equal? output null))
       ;can raise error
       (check_fifo1_full buffer next_inputs next_outputs)]
      
      [(and (equal? input null) (not (equal? output null)))
       (and (equal? buffer output) (check_fifo1_empty next_inputs next_outputs) )]
      
      [(and (not (equal? input null)) (not (equal? output null)))
       (and (equal? buffer output) (check_fifo1_empty next_inputs next_outputs) )]
   )
  )
 )
)

(define (check_fifo1_empty state1 state2)
  (if (null? state1)
      #t

   (let
      (
       [input (car state1)]
       [output (car state2)]
       [next_inputs (cdr state1)]
       [next_outputs (cdr state2)]
       )
    (cond
      [(and (equal? input null) (equal? output null))
       (check_fifo1_empty next_inputs next_outputs)]
      
      [(and (not (equal? input null)) (equal? output null))
       (check_fifo1_full input next_inputs next_outputs)]
      
      [(and (equal? input null) (not (equal? output null)))
       #f]
      ;can raise error
      [(and (not (equal? input null)) (not (equal? output null)))
       #f]
   )
  )
 )
)

(define (get_fifo1 input)
  (lambda
    (out)
    (
     let
        (
         [in (input)]
         )
      (if (check_fifo1_empty in out)
          out
          #f
          )
      
  )
  )
  )

(define (get_merger_input_channel input)
  (lambda
    (output)
    (foldl
     (lambda (x y prev) (and prev (or (equal? x null) (equal? x y))))
       #t
       (input)
       output)     
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
    (get_merger_node state (get_merger_input_channel source_node1) (get_merger_input_channel source_node2) validators)
  )
  )


(define get_join_block
  (lambda
      (state source_node1 source_node2 validators)
    (get_join_node state (get_sync source_node1) (get_sync source_node2) validators)
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


(define simple_dest1 (get_simple_dest (list 1 2 null 4 5) ))
(define simple_dest2 (get_simple_dest (list 1 2 3 4 5) ))
(define simple_dest3 (get_simple_source (list 1 6 3 4 null) ))
(define simple_dest4 (get_simple_source (list 9 3 3 4 null) ))
(define simple_dest5 (get_simple_source (list '(1 9) '(6 3) '(3 3) '(4 4) null) ))


(define valid_sync (get_sync (list simple_source1 simple_dest1 )))
(define invalid_sync (get_sync (list simple_source1 simple_dest2 )))
(define valid_sync_drain (get_sync_drain (list simple_source3 simple_dest4 )))
(define invalid_sync_drain (get_sync_drain (list simple_source1 simple_dest3 )))

;(define sync (get_sync simple_source1))
;(define dest (get_dest (list 1 2 null 4 5) sync))


(define
  (phase1 a b c)
  (
   let*
       (
        [node_a (get_simple_source a)]
        [core (get_replicator_node node_a)]
        [sync (get_sync node_a)]
        [sync_drain (get_sync_drain node_a)]
        [node_b (get_dest b (listify sync_drain))]
        [node_c (get_dest c (listify sync))]
        [nodes (list node_a node_b node_c)]
        )
     (check_nodes nodes)
    
   )
  )

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

