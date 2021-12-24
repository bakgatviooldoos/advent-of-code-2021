#lang racket

(define-values (enhancement-algorithm
                input-image)
  (with-input-from-file "day-20-1.txt"
    (lambda ()
      (define (->pixel px)
        (if (equal? #\# px) #\1 #\0))
        
      (define algorithm
        (map ->pixel (string->list (read-line))))
      (read-line)

      (define image
        (for/list ([l (in-lines)])
          (map ->pixel (string->list l))))

      (values algorithm image))))

(define (infinite-image base-image layer-count)
  (define zeroth-layer
    (let ([layer (make-hash)])
      (for ([row base-image]
            [i   (in-naturals 0)])
        (for ([px row]
              [j  (in-naturals 0)])
          (hash-set! layer (cons i j) px)))
      layer))

  (make-hash
   (cons (cons 0 zeroth-layer)
         (for/list ([index (in-range 1 (add1 layer-count))])
           (cons index (make-hash))))))

(define (enhance inf-image)
  (define (get-pixel index i j)
    (cond [(zero? index)
           (hash-ref! (hash-ref inf-image 0) (cons i j) #\0)]
          [else
           (hash-ref!
            (hash-ref inf-image index)
            (cons i j)
            (lambda ()
              (define super-pixel
                (for*/list ([oi (in-range -1 2)]
                            [oj (in-range -1 2)])
                  (get-pixel (sub1 index) (+ i oi) (+ j oj))))

              (list-ref enhancement-algorithm
                        (string->number (list->string super-pixel) 2))))]))

  (define (pixel->int px)
    (if (equal? #\1 px) 1 0))

  (define top-index
    (apply max (hash-keys inf-image)))

  (define (unseen-pixels min-i max-i min-j max-j)
    (+ (for/sum ([i (in-range (sub1 min-i) (add1 max-i))])
         (+ (pixel->int (get-pixel top-index i (sub1 min-j)))
            (pixel->int (get-pixel top-index i (add1 max-j)))))

       (for/sum ([j (in-range min-j max-j)])
         (+ (pixel->int (get-pixel top-index (sub1 min-i) j))
            (pixel->int (get-pixel top-index (add1 max-i) j))))))

  (define-values (min-i max-i
                  min-j max-j)
    (let* ([coords (hash-keys (hash-ref inf-image 0))]
           [is     (map car coords)]
           [js     (map cdr coords)])
      (values (- (apply min is) top-index) (+ (apply max is) top-index)
              (- (apply min js) top-index) (+ (apply max js) top-index))))

  (define seen-pixels
    (for*/sum ([i (in-range min-i (add1 max-i))]
               [j (in-range min-j (add1 max-j))])
      (pixel->int (get-pixel top-index i j))))

  (let loop ([min-i min-i] [max-i max-i]
             [min-j min-j] [max-j max-j]
             [seen seen-pixels])
    (define newly-seen
      (unseen-pixels min-i max-i min-j max-j))

    (if (zero? newly-seen)
        seen
        (loop (sub1 min-i) (add1 max-i)
              (sub1 min-j) (add1 max-j)
              (+ seen newly-seen)))))
             
(displayln (format "number of lit pixels after enhancing the input image x 2:\n~a"
 (enhance (infinite-image input-image 2))))
(newline)

(displayln (format "number of lit pixels after enhancing the input image x 50:\n~a"
 (enhance (infinite-image input-image 50))))