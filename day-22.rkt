#lang racket

(struct cuboid [x1 x2 y1 y2 z1 z2]
  #:transparent)

(define (volume a)
  (define-values (x-length y-length z-length)
    (match a
      [(cuboid x1 x2 y1 y2 z1 z2)
       (values (- (add1 x2) x1)
               (- (add1 y2) y1)
               (- (add1 z2) z1))]))
  (define absolute-volume
    (* (abs x-length) (abs y-length) (abs z-length)))
  
  (if (andmap positive? `(,x-length ,y-length ,z-length))
      absolute-volume
      (- absolute-volume)))

(define (overlap a b)
  (match* (a b)
    [((cuboid x1 x2 y1 y2 z1 z2)
      (cuboid u1 u2 v1 v2 w1 w2))
     (cuboid (max x1 u1) (min x2 u2)
             (max y1 v1) (min y2 v2)
             (max z1 w1) (min z2 w2))]))

(define reboot-steps
  (with-input-from-file "day-22-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (let ([split-state
               (string-split (string-replace l "," " ") " ")])
          (cons (equal? "on" (first split-state))
                (apply
                 cuboid
                 (apply append
                        (map (lambda (range)
                               (map string->number (string-split (substring range 2) "..")))
                             (rest split-state))))))))))

(define (reboot steps)
  (let loop ([states (list)]
             [steps  steps])
    (cond [(empty? steps) states]
          [else
           (define state-1 (first steps))
           (define case-1  (car state-1))
           (define boid-1  (cdr state-1))
           
           (define updated-states
             (apply append
                    (for/list ([state-2 states])
                      (define case-2    (car state-2))
                      (define boid-2    (cdr state-2))
                      (define intersect (overlap boid-1 boid-2))

                      (if (positive? (volume intersect))
                          (list state-2 (cons (not case-2) intersect))
                          (list state-2)))))
           (loop (if (not case-1)
                     updated-states
                     (cons state-1 updated-states))
                 (rest steps))])))

(define (sum-cuboids states)
  (for/sum ([state states])
    ((if (car state) + -) (volume (cdr state)))))

(define warmup-cuboid
  (cuboid -50 +50 -50 +50 -50 +50))

(displayln (format "number of cubes switched on after warmup reboot:\n~a"
 (sum-cuboids (reboot (filter (lambda (state)
                                (positive? (volume (overlap warmup-cuboid (cdr state)))))
                              reboot-steps)))))
(newline)

(displayln (format "number of cubes switched on after complete reboot:\n~a"
 (sum-cuboids (reboot reboot-steps))))