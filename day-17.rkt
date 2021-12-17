#lang racket

(define-values (x-range y-range)
  (with-input-from-file "day-17-1.txt"
    (lambda ()
      (define (parse-range range-string)
        (define split-start-end
          (string-split (substring range-string (string-length "x=")) ".."))

        (cons (string->number (first split-start-end))
              (string->number (second split-start-end))))
      
      (define split-x-y
        (string-split (substring (read-line) (string-length "target area: ")) ", "))

      (values (parse-range (first split-x-y))
              (parse-range (second split-x-y))))))

(define (crosses-target? initial-vx initial-vy)
  (let loop ([px 0]          [py 0]
             [vx initial-vx] [vy initial-vy])
    (cond [(or (< (cdr x-range) px)
               (< py (car y-range)))
           #f]
          [(and (<= (car x-range) px (cdr x-range))
                (<= (car y-range) py (cdr y-range)))
           #t]
          [else
           (loop (+ px vx)       (+ py vy)
                 (- vx (sgn vx)) (- vy 1))])))

(define valid-initial-velocities
 (for*/list ([vx (in-range 0             (add1 (cdr x-range)))]
             [vy (in-range (car y-range) (- (car y-range)))]
             #:when (crosses-target? vx vy))
   (cons vx vy)))

(define (apex initial-vy)
  (/ (* initial-vy (+ initial-vy 1)) 2))

(displayln (format "highest y-position reached in any valid trajectory:\n~a"
 (apex (cdr (argmax cdr valid-initial-velocities)))))
(newline)

(displayln (format "total number of valid trajectories:\n~a"
 (length valid-initial-velocities)))
