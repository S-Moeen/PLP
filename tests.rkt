#lang racket
(require "parts.rkt")
;validjoin

(define validjoin (get_dest (list '(1 9) '(6 3) '(3 3) '(4 4) null) (list (get_join_channel simple_source3 simple_source4 )) ) )
;invalid join
(define invalidjoin (get_dest (list '(1 9) '(3 3) '(3 3) '(4 4) null) (list (get_join_channel simple_source3 simple_source4 )) ) )

;valid sync
(define validsync (get_dest (list 1 2 3 4 5) (list (get_sync simple_source2)) ) )
;invalid sync
(define invalidsync (get_dest (list 1 2 3 4 3) (list (get_sync simple_source2)) ) )

;valid sync_drain
(define validsync_drain (get_dest (list 1 6 3 4 null) (list (get_sync_drain simple_source4 (set (list 1 6 3 4 null))) ) ))
;invalid sync_drain
(define invalidsync_drain (get_dest (list 1 6 null 4 null) (list (get_sync_drain simple_source4 (set (list 1 6 3 4 null))) ) ))

;valid merger
(define validmerger (get_dest (list 1 2 3 4 5) (list (get_merger_channel simple_source1 simple_source2  ) )))
;invalid merger
(define invalidmerger (get_dest (list 3 2 3 4 5) (list (get_merger_channel simple_source1 simple_source2  ) )))

;valid fifo1
(define validfifo1 (get_dest (list null null null 3 null) (list (get_fifo1 simple_source5) )))
;invalid fifo1
(define invalidfifo1 (get_dest (list null null null 4 null) (list (get_fifo1 simple_source5) )))

(define tests (list validjoin invalidjoin validsync invalidsync validsync_drain invalidsync_drain validmerger invalidmerger validfifo1 invalidfifo1))
;If everything works this should be empty set in even places and non empty list in odd places
(define result (map (lambda (x) (x)) tests))