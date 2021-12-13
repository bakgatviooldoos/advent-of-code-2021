#lang racket

(define signal-patterns
  (with-input-from-file "day-8-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (map (lambda (input-output)
               (map string->list (string-split input-output " ")))
             (string-split l " | "))))))

(define ZERO  (string->list "abcefg"))
(define ONE   (string->list "cf"))
(define TWO   (string->list "acdeg"))
(define THREE (string->list "acdfg"))
(define FOUR  (string->list "bcdf"))
(define FIVE  (string->list "abdfg"))
(define SIX   (string->list "abdefg"))
(define SEVEN (string->list "acf"))
(define EIGHT (string->list "abcdefg"))
(define NINE  (string->list "abcdfg"))

(define ZERO-to-NINE
  (list ZERO ONE TWO THREE FOUR FIVE SIX SEVEN EIGHT NINE))

(define (shared-segments pattern patterns)
  (group-by identity
            (sort (map (lambda (other)
                         (length (set-intersect pattern other)))
                       (remove pattern patterns))
                  <)))

(define shared-segments->number
  (for/hash ([digit  ZERO-to-NINE]
             [number (in-naturals)])
    (values (shared-segments digit ZERO-to-NINE) number)))

(define (pattern->number patterns)
  (for/hash ([pattern patterns])
    (define number
      (hash-ref shared-segments->number (shared-segments pattern patterns)))
    (values (sort pattern char<?) number)))

(define fixed-outputs
  (for/list ([input-output signal-patterns])
    (map (lambda (pattern)
           (hash-ref (pattern->number (first input-output))
                     (sort pattern char<?)))
         (second input-output))))

(define (numeral->number numeral)
  (for/sum ([n (reverse numeral)]
            [i (in-naturals)])
    (* n (expt 10 i))))

(displayln (format "number of times 1, 4, 7 or 8 appear in the fixed outputs:\n~a"
 (apply + (map (lambda (numeral)
                 (apply + (map (lambda (n)
                                 (if (member n '(1 4 7 8)) 1 0))
                               numeral)))
               fixed-outputs))))

(displayln (format "sum of outputs of the fixed 4-digit displays:\n~a"
 (apply + (map numeral->number fixed-outputs))))
