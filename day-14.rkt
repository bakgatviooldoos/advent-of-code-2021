#lang racket

(define-values (template-polymer insertion-rules)
  (with-input-from-file "day-14-1.txt"
    (lambda ()
      (define template
        (map string->symbol
             (filter non-empty-string? (string-split (read-line) ""))))
      (read-line)

      (define rules
        (for/hash ([l (in-lines)])
          (define split (string-split l " -> "))
          
          (define A (string->symbol (substring (first split) 0 1)))
          (define B (string->symbol (substring (first split) 1 2)))
          (define C (string->symbol (second split)))
          
          (values (cons A B) C)))
      (values template rules))))

(define (polymerize polymer rules #:repeat times)
  (define (pair-counts polymer)
    (define pair->count (make-hash))
    (let loop ([polymer polymer])
      (cond [(> 2 (length polymer)) pair->count]
            [else
             (define A (first polymer))
             (define B (second polymer))

             (hash-set! pair->count
                        (cons A B)
                        (add1 (hash-ref! pair->count (cons A B) 0)))
             (loop (cdr polymer))])))
  
  (let loop ([pair->count (pair-counts polymer)]
             [repeat      times])
    (cond [(zero? repeat) pair->count]
          [else
           (define next-pair->count (make-hash))
           (hash-for-each
            pair->count
            (lambda (pair count)
              (define rule  (hash-ref rules pair))
              (define pair1 (cons (car pair) rule))
              (define pair2 (cons rule (cdr pair)))
              (hash-set! next-pair->count
                         pair1
                         (+ count (hash-ref! next-pair->count pair1 0)))
              (hash-set! next-pair->count
                         pair2
                         (+ count (hash-ref! next-pair->count pair2 0)))))
           (loop next-pair->count (sub1 repeat))])))

(define (count-elements pair->count)
  (define element->count (make-hash))
  (hash-for-each
   pair->count
   (lambda (pair count)
     (hash-set! element->count
                (car pair)
                (+ count
                   (hash-ref! element->count (car pair) 0)))
     (hash-set! element->count
                (cdr pair)
                (+ count
                   (hash-ref! element->count (cdr pair) 0)))))

  (for/hash ([element (hash-keys element->count)])
    (define count (hash-ref element->count element))
    (values element
            (/ (+ count (if (even? count) 0 1)) 2))))


(displayln (format (string-append "difference in quantity of most common and least common\n"
                                  "elements after 10 polymerizations:\n~a")
 (let* ([polymerized
         (polymerize template-polymer insertion-rules #:repeat 10)]
        [counts
         (count-elements polymerized)])
   (- (apply max (hash-values counts)) (apply min (hash-values counts))))))
(newline)

(displayln (format (string-append "difference in quantity of most common and least common\n"
                                  "elements after 40 polymerizations:\n~a")
 (let* ([polymerized
         (polymerize template-polymer insertion-rules #:repeat 40)]
        [counts
         (count-elements polymerized)])
   (- (apply max (hash-values counts)) (apply min (hash-values counts))))))
