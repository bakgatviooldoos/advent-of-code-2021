#lang racket

(define lanternfish-ages
  (with-input-from-file "day-6-1.txt"
    (lambda ()
      (define age-groups
        (map string->number (string-split (read-line) ",")))
      
      (for/hash ([age (in-range 9)])
        (define group? (findf (lambda (group)
                                (equal? age (first group)))
                              (group-by identity age-groups)))
        (values age (if (not group?) 0 (length group?)))))))

(define (age-one-day age-groups)
  (for/hash ([age (hash-keys age-groups)])
    (values age (+ (hash-ref age-groups (modulo (add1 age) (hash-count age-groups)))
                   (if (equal? 6 age) (hash-ref age-groups 0) 0)))))

(define (age-by fish-ages days)
  (if (zero? days)
      fish-ages
      (age-by (age-one-day fish-ages) (sub1 days))))

(define (total-fish age-groups)
  (apply + (hash-values age-groups)))

(displayln (format "total number of lanternfish after 80 days:\n~a"
 (total-fish (age-by lanternfish-ages 80))))
(newline)

(displayln (format "total number of lanternfish after 256 days:\n~a"
 (total-fish (age-by lanternfish-ages 256))))
