#lang racket

(define heightmap
  (with-input-from-file "day-9-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
          (map string->number (filter non-empty-string? (string-split l "")))))))

(define (get-height heights i j)
  (list-ref (list-ref heights i) j))

(define (lowest-point? heights i j)
  (define point (get-height heights i j))
  (define up    (get-height heights (- i 1) j))
  (define right (get-height heights i (+ j 1)))
  (define down  (get-height heights (+ i 1) j))
  (define left  (get-height heights i (- j 1)))

  (and (< point up) (< point right) (< point down) (< point left)))

(define lowest-points
  (let ([aug-heights
         (append (list (append `(,0) (map add1 (first heightmap)) `(,0)))
                 (map (lambda (row)
                        (append `(,(add1 (first row))) row `(,(add1 (last row)))))
                      heightmap)
                 (list (append `(,0) (map add1 (last heightmap)) `(,0))))])
    (for*/list ([i (in-range 1 (add1 (length heightmap)))]
                [j (in-range 1 (add1 (length (first heightmap))))]
                #:when (lowest-point? aug-heights i j))
      (cons (sub1 i) (sub1 j)))))

(define basins
  (let ([aug-heights
         (append (list (make-list (+ 2 (length (first heightmap))) 9))
                 (map (lambda (row)
                        (append `(,9) row `(,9)))
                      heightmap)
                 (list (make-list (+ 2 (length (first heightmap))) 9)))]
        [known-points (mutable-set)])
    (define (map-basin-around i j)
      (define height (get-height aug-heights i j))
      (set-add! known-points (cons i j))
      (cond [(equal? 9 height) (list)]
            [else
             (cons height
                   (apply append
                          (for/list ([oi '(-1 +0 +1 +0)]
                                     [oj '(+0 +1 +0 -1)]
                                     #:unless (set-member? known-points (cons (+ i oi) (+ j oj))))
                            (map-basin-around (+ i oi) (+ j oj)))))]))
    (map (lambda (point)
           (map-basin-around (add1 (car point)) (add1 (cdr point))))
         lowest-points)))

(displayln (format "sum of the risk levels of the lowest points:\n~a"
 (apply + (map (lambda (point)
                 (add1 (get-height heightmap
                                  (car point)
                                  (cdr point))))
               lowest-points))))
(newline)

(displayln (format "product of the sizes of the 3 largest basins:\n~a"
 (apply * (map length
               (take (sort basins #:key length >) 3)))))
