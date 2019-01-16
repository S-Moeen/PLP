#lang racket

(define null 'null)
(define (get_sync inputs)
  ;it gets its input and output in a list as nodes
  (lambda
    ()
    (
     let
        (
         [in ((car inputs))]
         [out ((cadr inputs))]
         )
      (if (foldl (lambda (a b prev) (and prev (equal? a b)) ) #t in out)
          out
          #f
     )
  )
  )
  )

(define (get_sync_drain inputs)
  ;it gets its input and output in a list as nodes
  (lambda
    ()
    (
     let
        (
         [in ((car inputs))]
         [out ((cadr inputs))]
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

(define (get_replicator inputs)
  ;it gets its input and two outputs
  (lambda
    ()
    (
     let
        (
         [in ((car inputs))]
         [out1 ((cadr inputs))]
         [out2 ((caddr inputs))]
         )
      (if (foldl (lambda (i o1 o2 prev)
                   (and prev
                        (equal? i o1)
                        (equal? i o2)
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


(define simple_source1 (get_simple_source (list 1 2 null 4 5) ))
(define simple_source2 (get_simple_source (list 1 2 3 4 5) ))
(define simple_source3 (get_simple_source (list 1 6 3 4 null) ))
(define simple_source4 (get_simple_source (list 9 3 3 4 null) ))

(define simple_dest1 (get_simple_dest (list 1 2 null 4 5) ))
(define simple_dest2 (get_simple_dest (list 1 2 3 4 5) ))
(define simple_dest3 (get_simple_source (list 1 6 3 4 null) ))
(define simple_dest4 (get_simple_source (list 9 3 3 4 null) ))


(define valid_sync (get_sync (list simple_source1 simple_dest1 )))
(define invalid_sync (get_sync (list simple_source1 simple_dest2 )))

(define valid_sync_drain (get_sync_drain (list simple_source3 simple_dest4 )))
(define invalid_sync_drain (get_sync_drain (list simple_source1 simple_dest3 )))


