#lang racket

(define (hex->binary v)
  (~a (number->string (string->number v 16) 2)
      #:min-width 4 #:pad-string "0" #:align 'right))

(define BITS-transmission
  (with-input-from-file "day-16-1.txt"
    (lambda ()
      (define nibbles
        (map hex->binary (filter non-empty-string? (string-split (read-line) ""))))
      (string->list (apply string-append nibbles)))))

(define (bits->number b)
  (string->number (list->string b) 2))

(define ($packet input)
  (values (bits->number (take input 3))
          (bits->number (take (drop input 3) 3))
          (drop input 6)))

(define ($literal input)
  (let loop ([input input]
             [parts (list)])
    (cond [(equal? #\0 (first input))
           (define value
             (reverse (cons (cdr (take input 5)) parts)))
           (values (bits->number (apply append value)) (drop input 5))]
          [else
           (loop (drop input 5) (cons (cdr (take input 5)) parts))])))

(define ($operator input)
  (cond [(equal? #\0 (first input))
         (values 'sub-packets-bits
                 (bits->number (cdr (take input 16)))
                 (drop input 16))]
        [(equal? #\1 (first input))
         (values 'sub-packets-count
                 (bits->number (cdr (take input 12)))
                 (drop input 12))]))

(define (parse-packet input)
  (cond [(empty? input) (values 'eop input)]
        [else
         (define-values (version type-id content-bits)
           ($packet input))

         (cond [(equal? 4 type-id)
                (define-values (literal-value rest)
                  ($literal content-bits))
                (values (list type-id version literal-value)
                        rest)]
               [else
                (define-values (count-id count content)
                  ($operator content-bits))

                (cond [(equal? 'sub-packets-bits count-id)
                       (define-values (sub-packets rest)
                         (let loop ([input (take content count)]
                                    [parts (list)])
                           (cond [(empty? input)
                                  (values (reverse parts) (drop content count))]
                                 [else
                                  (define-values (packet rest)
                                    (parse-packet input))
                                  (loop rest (cons packet parts))])))
                       (values (list type-id version sub-packets)
                               rest)]
                      [(equal? 'sub-packets-count count-id)
                       (define-values (sub-packets rest)
                         (let loop ([input content]
                                    [parts (list)])
                           (cond [(equal? count (length parts))
                                  (values (reverse parts) input)]
                                 [else
                                  (define-values (packet rest)
                                    (parse-packet input))
                                  (loop rest (cons packet parts))])))
                       (values (list type-id version sub-packets)
                               rest)])])]))

(define (sum-versions ast)
  (match ast
    [`(4 ,version ,value)
     version]
    [`(,type-id ,version ,sub-packets)
     (+ version (apply + (map sum-versions sub-packets)))]))

(define (evaluate ast)
  (match ast
    [`(0 ,version ,sub-packets)
     (apply + (map evaluate sub-packets))]
    [`(1 ,version ,sub-packets)
     (apply * (map evaluate sub-packets))]
    [`(2 ,version ,sub-packets)
     (apply min (map evaluate sub-packets))]
    [`(3 ,version ,sub-packets)
     (apply max (map evaluate sub-packets))]
    [`(4 ,version ,value)
     value]
    [`(5 ,version ,sub-packets)
     (if (apply > (map evaluate sub-packets)) 1 0)]
    [`(6 ,version ,sub-packets)
     (if (apply < (map evaluate sub-packets)) 1 0)]
    [`(7 ,version ,sub-packets)
     (if (apply = (map evaluate sub-packets)) 1 0)]))

(define-values (parsed-ast rest)
  (parse-packet BITS-transmission))

(displayln (format "sum of the versions of the packets in the transmission:\n~a"
 (sum-versions parsed-ast)))
(newline)

(displayln (format "result of evaluating the packets in the transmission:\n~a"
 (evaluate parsed-ast)))