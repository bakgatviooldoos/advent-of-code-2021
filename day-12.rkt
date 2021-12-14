#lang racket

(define adjacent-caves
  (with-input-from-file "day-12-1.txt"
    (lambda ()
      (define cave->cave (make-hash))
      
      (define (connect-caves a b)
        (hash-set! cave->cave a (cons b (hash-ref! cave->cave a (list))))
        (hash-set! cave->cave b (cons a (hash-ref! cave->cave b (list)))))
      
      (for ([l (in-lines)])
        (apply connect-caves (string-split l "-")))
      cave->cave)))

(define (downcase? str)
  (equal? str (string-downcase str)))

(define (find-paths-in adjacencies #:where valid-transition?)
  (define paths (map (lambda (cave)
                       (list cave "start"))
                     (hash-ref adjacencies "start")))
  
  (let loop ([paths paths])
    (define extended 0)
    (define extended-paths
      (for/list ([path paths])
        (cond [(equal? "end" (car path)) (list path)]
              [else
               (set! extended (add1 extended))
               (for/list ([next-cave (hash-ref adjacencies (car path))]
                          #:when (valid-transition? next-cave path))
                 (cond [(and (downcase? next-cave)
                             (member next-cave path))
                        (cons next-cave (cons 'twice path))]
                       [(equal? "end" next-cave)
                        (cons next-cave (remove 'twice path))]
                       [else
                        (cons next-cave path)]))])))
    (if (zero? extended)
        paths
        (loop (apply append extended-paths)))))

(define transition-fun1
  (lambda (next path)
    (not (and (downcase? next) (member next path)))))

(define transition-fun2
  (lambda (next path)
    (or (transition-fun1 next path)
        (and (not (member next '("start" "end")))
             (member next path)
             (not (member 'twice path))))))

(displayln (format "number of paths that visit each small cave at most once:\n~a"
 (length (find-paths-in adjacent-caves #:where transition-fun1))))
(newline)

(displayln (format (string-append "number of paths that visit at most one small cave twice,\n"
                                  "except \"start\" and \"end\", and every other small cave at most once:\n~a")
 (length (find-paths-in adjacent-caves #:where transition-fun2))))
