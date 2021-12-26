#lang racket

(define scanner->beacons
  (with-input-from-file "day-19-1.txt"
    (lambda ()
      (define (parse-point coordinate-string)
        (map string->number (string-split coordinate-string ",")))

      (define scanner->beacons (make-hash))
        
      (let loop ([scanner 0]
                 [beacons (list)]
                 [line    (read-line)])
        (cond [(eof-object? line)
               (hash-set! scanner->beacons scanner beacons)
               scanner->beacons]
              [(equal? "" line)
               (hash-set! scanner->beacons scanner beacons)
               (loop (add1 scanner)
                     (list)
                     (read-line))]
              [(string-contains? line "scanner")
               (loop scanner
                     beacons
                     (read-line))]
              [else
               (loop scanner
                     (cons (parse-point line) beacons)
                     (read-line))])))))

(define orientations
  (let ([axis-perms (permutations `(,first ,second ,third))]
        [sign-perms (for*/list ([sx `(,+ ,-)]
                                [sy `(,+ ,-)]
                                [sz `(,+ ,-)])
                      (list sx sy sz))])
    (for*/list ([axes  axis-perms]
                [signs sign-perms])
      (cons axes signs))))

(define (orient points direction)
  (map (lambda (point)
         (map (lambda (axis sign)
                (sign (axis point)))
              (car direction) (cdr direction)))
         points))

(define (squared-distance a b)
  (+ (sqr (- (first a)  (first b)))
     (sqr (- (second a) (second b)))
     (sqr (- (third a)  (third b)))))

(define (manhatten-distance a b)
  (+ (abs (- (first a)  (first b)))
     (abs (- (second a) (second b)))
     (abs (- (third a)  (third b)))))

(define (add point points)
  (map (lambda (other)
         (map + other point))
       points))

(define (sub point points)
  (map (lambda (other)
         (map - other point))
       points))

(define signatures
  (for/hash ([scanner (hash-keys scanner->beacons)])
    (define points (hash-ref scanner->beacons scanner))
    (values scanner
            (for*/list ([a points]
                        [b points]
                        #:unless (equal? a b))
              (squared-distance a b)))))

(define ORIGIN (list 0 0 0))

(define-values (scanners
                oriented-beacons)
  (let ([scanners
         (mutable-set ORIGIN)]
        [scanner-count
         (hash-count scanner->beacons)]
        [oriented
         (make-hash (list (cons 0 (hash-ref scanner->beacons 0))))]
        [translated
         (for/hash ([scanner (hash-keys scanner->beacons)]
                    #:unless (zero? scanner))
           (define points
             (hash-ref scanner->beacons scanner))
           (values scanner
                   (for/list ([point points])
                     (sub point (cons ORIGIN points)))))]
        [correlated
         (for/hash ([scanner-1 (hash-keys signatures)])
           (values scanner-1
                   (filter (lambda (scanner-2)
                             (and (not (equal? scanner-1 scanner-2))
                                  (<= 66 (length (set-intersect (hash-ref signatures scanner-1)
                                                                (hash-ref signatures scanner-2))))))
                           (hash-keys signatures))))])
                          
    (let loop ()
      (define next-oriented
        (for*/or ([scanner-1 (hash-keys oriented)]
                  [scanner-2 (hash-ref correlated scanner-1)]
                  #:unless (hash-has-key? oriented scanner-2))
          (for*/or ([point     (hash-ref oriented scanner-1)]
                    [translate (hash-ref translated scanner-2)]
                    [direction orientations])
            (define transform
              (add point (orient translate direction)))

            (and (<= 12 (length (set-intersect transform (hash-ref oriented scanner-1))))
                 (list scanner-2 (first transform) (rest transform))))))
      (set-add!
       scanners
       (second next-oriented))
      
      (hash-set!
       oriented
       (first next-oriented)
       (third next-oriented))
    
      (if (equal? scanner-count (hash-count oriented))
          (values scanners
                  (remove-duplicates
                   (apply append (hash-values oriented))))
          (loop)))))

(displayln (format "total number of beacons among all scanners:\n~a"
 (length oriented-beacons)))
(newline)

(displayln (format "greatest manhatten distance between any two scanners:\n~a"
 (apply max (for*/list ([a scanners]
                        [b scanners]
                        #:unless (equal? a b))
              (manhatten-distance a b)))))
