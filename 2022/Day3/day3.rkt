#lang racket

(require racket/cmdline)
(require racket/set)

(define items "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define priorities
  (make-hash
   (for/list
       ([i (in-range (string-length items))])
     (cons (string-ref items i) (+ i 1)))))

; TODO: Do it recursivelly
(define (part1 lines)
  (define total 0)
  (for ([line lines])
    (define size (string-length line))
    (define r1
      (string->list (substring line 0 (/ size 2))))
    (define r2
      (string->list (substring line (/ size 2) size)))
    (define common (set-intersect r1 r2))
    (for
        ([c common])
      (set! total (+ total (hash-ref priorities c)))))
  total)

; TODO: Do it recursivelly
(define (part2 lines)
  (define total 0)
  (define groupSize 0)
  (define badgeSet null)
  (for ([line lines])
    (cond
      [(equal? groupSize 0)
       (set! badgeSet (string->list line))]
      [(equal? groupSize 2)
       (set! badgeSet (set-intersect badgeSet (string->list line)))
       (for
           ([c badgeSet])
         (set! total (+ total (hash-ref priorities c))))
       (set! badgeSet null) ]
      [else
       (set! badgeSet (set-intersect badgeSet (string->list line)))])
    (set! groupSize (modulo (+ groupSize 1) 3)))
  total)

(command-line
 #:program "day3"
 #:args (filename part)

 (define lines
   (port->lines (open-input-file filename) #:close? #t))

 (if (equal? part "1")
     (part1 lines)
     (part2 lines)))
