#lang racket

(define risk-levels
  (with-input-from-file "day-15-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (map string->number (filter non-empty-string? (string-split l "")))))))

(define (least-risky-path risk-levels)
  (define size (length risk-levels))

  (define risk-counters
    (list->vector (map (lambda (row)
                         (list->vector row))
                       risk-levels)))
  (vector-set! (vector-ref risk-counters 0) 0 '(0 0))
  
  (define (get-risk i j)
    (list-ref (list-ref risk-levels i) j))

  (define (left-up-right-down i j)
      (for/list ([oi '(+0 -1 +0 +1)]
                 [oj '(-1 +0 +1 +0)]
                 #:when (and (<= 0 (+ i oi) (sub1 size))
                             (<= 0 (+ j oj) (sub1 size))))
        (cons (+ i oi) (+ j oj))))
  
  (let loop ([updated-risks
              (mutable-set (cons 0 1) (cons 1 0))])
    (define (get-counter i j)
      (vector-ref (vector-ref risk-counters i) j))

    (define (relevant-counters conns)
      (filter-map (lambda (conn)
                    (define counter (get-counter (car conn) (cdr conn)))
                    (if (list? counter) counter #f))
                  conns))

    (define (extend-updated-risks conns)
      (set-union! updated-risks (list->set conns)))

    (define (minimize-risk i j)
      (define counter (get-counter i j))
      
      (cond [(list? counter)
             (set-remove! updated-risks (cons i j))  
             (define conns    (left-up-right-down i j))
             (define counters (relevant-counters conns))
             
             (cond [(empty? counters) counter]
                   [else
                    (define minimal-risks (argmin car counters))
                    (define risk-here     (get-risk i j))
                    (define total-risk    (+ risk-here (car minimal-risks)))
                    
                    (cond [(<= (car counter) total-risk) counter]
                          [else
                           (extend-updated-risks conns)
                           (cons total-risk (cons risk-here (cdr minimal-risks)))])])]
            [(zero? counter)
             (define conns    (left-up-right-down i j))
             (define counters (relevant-counters conns))
             
             (cond [(empty? counters) counter]
                   [else
                    (set-remove! updated-risks (cons i j))
                    (extend-updated-risks conns)
                           
                    (define minimal-risks (argmin car counters))
                    (define risk-here     (get-risk i j))
                    (define total-risk    (+ risk-here (car minimal-risks)))
                    
                    (cons total-risk (cons risk-here (cdr minimal-risks)))])]
            [else
             (define counters
               (relevant-counters (left-up-right-down i j)))
             (if (empty? counters) counter (sub1 counter))]))
    (set-for-each
     updated-risks
     (lambda (conn)
       (vector-set!
        (vector-ref risk-counters (car conn))
        (cdr conn)
        (minimize-risk (car conn) (cdr conn)))))
    
    (if (set-empty? updated-risks)
        (vector-ref (vector-ref risk-counters (sub1 size)) (sub1 size))
        (loop updated-risks))))

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
