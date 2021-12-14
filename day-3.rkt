#lang racket

(define diagnostic-report
  (with-input-from-file "day-3-1.txt"
    (lambda ()
      (for/list ([l (in-lines)])
        (map string->number (filter non-empty-string? (string-split l "")))))))

(define (bits->integer bits)
  (string->number (apply string-append (map ~a bits)) 2))

(define (count-bits report)
  (foldl (lambda (line counts)
           (map (lambda (bit count)
                  (if (zero? bit)
                      (cons (add1 (car count)) (cdr count))
                      (cons (car count) (add1 (cdr count)))))
                line counts))
         (make-list (length (car report)) (cons 0 0))
         report))

(define (most-common-bit bit-count)
  (if (<= (car bit-count) (cdr bit-count)) 1 0))

(define (least-common-bit bit-count)
  (- 1 (most-common-bit bit-count)))

(define (power-usage-rating report)
  (define bit-counts (count-bits report))
  (define gamma-rate (bits->integer (map most-common-bit bit-counts)))
  (* gamma-rate
     (- (expt 2 (length bit-counts)) gamma-rate 1)))

(define (life-support-rating report)
  (define (rating-filter pred report)
    (let loop ([index  0]
               [report report])
      (cond [(empty? (cdr report)) (car report)]
            [else
             (define bit-count (list-ref (count-bits report) index))
             (loop (add1 index)
                   (filter (lambda (line)
                             (equal? (pred bit-count) (list-ref line index)))
                           report))])))
  (define oxygen-rating
    (bits->integer
      (rating-filter most-common-bit report)))
  
  (define scrubber-rating
    (bits->integer
      (rating-filter least-common-bit report)))
    
  (* oxygen-rating
     scrubber-rating))

(displayln (format "power usage rating:\n~a"
 (power-usage-rating diagnostic-report)))
(newline)

(displayln (format "life-support rating:\n~a"
 (life-support-rating diagnostic-report)))
