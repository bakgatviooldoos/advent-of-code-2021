#lang racket

(define octopus-energy-levels
  (with-input-from-file "day-11-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
          (map string->number (filter non-empty-string? (string-split l "")))))))

(define H (length octopus-energy-levels))
(define W (length (first octopus-energy-levels)))

(define (get-level energy-levels i j)
  (list-ref (list-ref energy-levels i) j))

(define (neighbourhood i j)
  (for/list ([oi '(+1 +1 +1 +0 -1 -1 -1 +0)]
             [oj '(-1 +0 +1 +1 +1 +0 -1 -1)]
             #:when (and (<= 0 (+ i oi) (sub1 H))
                         (<= 0 (+ j oj) (sub1 W))))
    (cons (+ i oi) (+ j oj))))

(define (adjacent-flashes energy-levels i j)
  (for/sum ([neighbour (neighbourhood i j)])
    (define ni (car neighbour))
    (define nj (cdr neighbour))
    (if (< 9 (get-level energy-levels ni nj)) 1 0)))

(define (simulate-step energy-levels)
  (define part-1
    (for/list ([row energy-levels])
      (map add1 row)))

  (define part-1->2
    (let loop ([energy-levels part-1]
               [total-flashes 0])
      (define flashcount 0)
      (define do-flashes
        (for/list ([i H])
          (for/list ([j W])
            (define level
              (get-level energy-levels i j))
            (cond [(< 9 level)
                   (set! flashcount (add1 flashcount))
                   -inf.0]
                  [else
                   (+ level (adjacent-flashes energy-levels i j))]))))
        (if (zero? flashcount)
            (cons do-flashes total-flashes)
            (loop do-flashes (+ total-flashes flashcount)))))

  (define part-2->3
    (for/list ([row (car part-1->2)])
      (map (lambda (level)
             (if (equal? -inf.0 level) 0 level))
           row)))
  (cons part-2->3 (cdr part-1->2)))

(define (simulate steps energy-levels [flashes 0])
  (cond [(zero? steps)
         (cons energy-levels flashes )]
        [else
         (define one-step
           (simulate-step energy-levels))

         (simulate (sub1 steps) (car one-step) (+ flashes (cdr one-step)))]))

(define (find-synchronisation energy-levels [steps 0])
  (cond [(andmap (lambda (row)
                   (andmap zero? row))
                 energy-levels)
         (cons energy-levels steps)]
        [else
         (define one-step
           (simulate-step energy-levels))
         (find-synchronisation (car one-step) (add1 steps))]))

(displayln (format "number of flashes after 100 steps:\n~a"
 (cdr (simulate 100 octopus-energy-levels))))

(displayln (format "number of steps until octopuses synchronise:\n~a"
 (cdr (find-synchronisation octopus-energy-levels))))

