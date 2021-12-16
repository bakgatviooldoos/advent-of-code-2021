#lang racket

(define risk-levels
  (with-input-from-file "day-15-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (map string->number (filter non-empty-string? (string-split l "")))))))

(define (least-risky-path risk-levels)
  (define size (length risk-levels))

  (define (get-risk i j)
    (list-ref (list-ref risk-levels i) j))

  (define risk-counters
    (list->vector (map (lambda (row)
                         (list->vector row))
                       risk-levels)))
  (vector-set! (vector-ref risk-counters 0) 0 '(0 0))

  (define (get-counter i j)
    (vector-ref (vector-ref risk-counters i) j))

  (define (get-list-counters positions)
    (filter-map (lambda (position)
                  (define counter (get-counter (car position) (cdr position)))
                  (if (list? counter) counter #f))
                positions))

  (define (left-up-right-down i j)
    (for/list ([oi '(+0 -1 +0 +1)]
               [oj '(-1 +0 +1 +0)]
               #:when (and (<= 0 (+ i oi) (sub1 size))
                           (<= 0 (+ j oj) (sub1 size))))
      (cons (+ i oi) (+ j oj))))
  
  (let loop ([invalidated
              (mutable-set (cons 0 1) (cons 1 0))])
    (define (add-invalidated positions)
      (set-union! invalidated (list->set positions)))

    (define (minimize-risk i j)
      (define this-counter (get-counter i j))
      
      (cond [(list? this-counter)
             (set-remove! invalidated (cons i j))  
             (define adj-positions (left-up-right-down i j))
             (define adj-counters  (get-list-counters adj-positions))
             
             (cond [(empty? adj-counters) this-counter]
                   [else
                    (define minimal-risks (argmin car adj-counters))
                    (define this-risk     (get-risk i j))
                    (define total-risk    (+ this-risk (car minimal-risks)))
                    
                    (cond [(<= (car this-counter) total-risk) this-counter]
                          [else
                           (add-invalidated adj-positions)
                           (cons total-risk (cons this-risk (cdr minimal-risks)))])])]
            [(zero? this-counter)
             (define adj-positions (left-up-right-down i j))
             (define adj-counters  (get-list-counters adj-positions))
             
             (cond [(empty? adj-counters) this-counter]
                   [else
                    (set-remove! invalidated (cons i j))
                    (add-invalidated adj-positions)
                           
                    (define minimal-risks (argmin car adj-counters))
                    (define this-risk     (get-risk i j))
                    (define total-risk    (+ this-risk (car minimal-risks)))
                    
                    (cons total-risk (cons this-risk (cdr minimal-risks)))])]
            [else
             (define adj-counters
               (get-list-counters (left-up-right-down i j)))
             (if (empty? adj-counters) this-counter (sub1 this-counter))]))
    (set-for-each
     invalidated
     (lambda (position)
       (vector-set!
        (vector-ref risk-counters (car position))
        (cdr position)
        (minimize-risk (car position) (cdr position)))))
    
    (if (set-empty? invalidated)
        (vector-ref (vector-ref risk-counters (sub1 size)) (sub1 size))
        (loop invalidated))))

(define (displace-risk r m n)
  (define-values (a b)
    (let ([u (+ m r)]
          [v (+ n r)])
      (values (if (< 9 u) (- u 9) u)
              (if (< 9 v) (- v 9) v))))
  (define c (modulo (+ a b (- r)) 9))
  (if (zero? c) 9 c))

(define (combine list-of-lists)
  (if (empty? (car list-of-lists))
      (list)
      (cons (apply append (map car list-of-lists))
            (combine (map cdr list-of-lists)))))

(define expanded-risk-levels
  (apply append
         (for/list ([m 5])
           (combine (for/list ([n 5])
                      (map (lambda (risks)
                             (map (lambda (risk) (displace-risk risk m n))
                                  risks))
                           risk-levels))))))

(displayln (format "total risk of the least risky path through the initial map:\n~a"
 (first (least-risky-path risk-levels))))
(newline)

(displayln (format "total risk of the least risky path through the extended map:\n~a"
 (first (least-risky-path expanded-risk-levels))))
