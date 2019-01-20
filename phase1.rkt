#lang racket
(require "parts.rkt")
(require (prefix-in io:"io.rkt"))

(define
  (simulate lst)
  (
   let*
       (
        [a (car lst)]
        [b (cadr lst)]
        [c (caddr lst)]
        [node_a (get_simple_source a)]
        [core (get_replicator_node node_a)]
        [sync (get_sync node_a)]
        [sync_drain (get_sync_drain node_a (setify b))]
        [node_b (get_dest b (list sync_drain))]
        [node_c (get_dest c (list sync))]
        [nodes (list node_a node_b node_c)]
        )
     (check_nodes nodes)

    
   )
  )

(define
  (phase1)
  (
   io:write_output
   (map simulate (io:read_parse "DATA_SEC1.txt" 3))
   "RESULT_SEC1.txt"
   )
  )