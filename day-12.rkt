#lang racket

(define adjacent-caves
  (with-input-from-file "day-12-1.txt"
    (lambda ()
      (define cave->cave (make-hash))
      
      (define (add-bipath a b)
        (if (hash-has-key? cave->cave a)
            (hash-set! cave->cave a (cons b (hash-ref cave->cave a)))
            (hash-set! cave->cave a (cons b (list))))
        (if (hash-has-key? cave->cave b)
            (hash-set! cave->cave b (cons a (hash-ref cave->cave b)))
            (hash-set! cave->cave b (cons a (list)))))
      
      (for ([l (in-lines)])
        (define split (string-split l "-"))
        (add-bipath (first split) (second split)))
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
      (apply append
             (for/list ([path paths])
               (cond [(equal? "end" (car path))
                      (list path)]
                     [else
                      (set! extended (add1 extended))
                      
                      (define next-caves
                        (hash-ref adjacencies (car path)))
                      (for/list ([next next-caves]
                                 #:when (valid-transition? next path))
                        (cond [(and (downcase? next)
                                    (member next path))
                               (cons next (cons 'twice path))]
                              [(equal? "end" next)
                               (cons next (remove 'twice path))]
                              [else
                               (cons next path)]))]))))
    
    (if (zero? extended)
        paths
        (loop extended-paths))))

(define transition-fun1
  (lambda (next path)
    (not (and (downcase? next) (member next path)))))

(define transition-fun2
  (lambda (next path)
    (or (and (not (member next '("start" "end")))
             (member next path)
             (not (member 'twice path)))
        (not (and (member next path) (downcase? next))))))

(displayln (format "number of paths that visit each small cave at most once:\n~a"
 (length (find-paths-in adjacent-caves #:where transition-fun1))))

(displayln (format (string-append "number of paths that visit at most one small cave twice,\n"
                                  "except \"start\" and \"end\", and every other small cave at most once:\n~a")
 (length (find-paths-in adjacent-caves #:where transition-fun2))))
