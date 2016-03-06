(use extras)

(define my-last
  (lambda (list)
    (cond
     ((null? list) '())
     ((null? (cdr list)) (car list))
     (else (my-last (cdr list))))))

(define my-but-last
  (lambda (list)
    (cond
     ((null? list) '())
     ((null? (cdr (cdr list))) list)
     (else (my-but-last (cdr list))))))

(define element-at
  (lambda (list pos)
    (cond
     ((null? list) '())
     ((<= pos 0) '())
     ((= pos 1) (car list))
     (else (element-at (cdr list) (- pos 1))))))

(define count-elements
  (lambda (list)
    (cond
     ((null? list) 0)
     (else (+ 1 (count-elements (cdr list)))))))

(define (my-reverse list) ; Revisit - Rule: Use accumulators and subroutines
  (define (my-reverse-2 list acc)
    (cond
     ((null? list) acc)
     (else
      (my-reverse-2 (cdr list) (cons (car list) acc))
      )))
  (my-reverse-2 list '()))

(define (my-palindrome? list) ; !
  (define (compare? list rev)
    (cond
     ((null? list) #t)
     ((not (eqv? (car list) (car rev))) #f)
     (else (compare? (cdr list) (cdr rev)))
     ))
  (compare? list (my-reverse list)))

(define (my-flatten lst) ; Revisit - Rule: Both arguments to append can be calls!
  (cond
   ((null? lst) '())
   ((list? (car lst))
    (append (my-flatten (car lst)) (my-flatten (cdr lst))))
   (else (cons (car lst) (my-flatten (cdr lst))))))

(define (rem-dupes lst) ; !
  (define (inside-rem lst last)
    (cond
     ((null? lst) '())
     ((eqv? (car lst) last) (inside-rem (cdr lst) last))
     (else (cons (car lst) (inside-rem (cdr lst) (car lst))))
     ))
  (inside-rem lst #f))

(define (pack-dupes lst) ; ! Note: Last thing you do (pack the last acc, in this case) must go in base!
  (define (inside-pack lst last acc)
    (cond
     ((null? lst) (cons acc '()))
     ((eqv? (car lst) last) (inside-pack (cdr lst) (car lst) (cons (car lst) acc)))
     (else (cons acc (inside-pack (cdr lst) (car lst) (cons (car lst) '()))))
     ))
  (inside-pack lst (car lst) '()))

(define (encode lst) ; ! Improvement: Mod algorithm so there's no need to call my-reverse (maybe no accum)
  (let ((packed (my-reverse (pack-dupes lst))))
    (define (inner-encode lst acc)
      (cond
       ((null? lst) (cons acc '()))
       (else
        (inner-encode (cdr lst)
                      (cons
                       (cons (count-elements (car lst)) (cons (car (car lst)) '()))
                       acc)))
       )
      )
    (inner-encode packed '())
    )
  )

(define (encode-modified lst)
  (let ((packed (my-reverse (pack-dupes lst))))
    (define (inner-encode lst acc)
      (cond
       ((null? lst) (cons acc '()))
       ((eq? 1 (count-elements (car lst))) (inner-encode (cdr lst) (cons (car (car lst)) acc)))
       (else 
        (inner-encode (cdr lst)
                      (cons
                       (cons (count-elements (car lst)) (cons (car (car lst)) '()))
                       acc)))
       )
      )
    (inner-encode packed '())
    )
  )

(define explode ; Takes a list (not dotted pair) of '(<fixnum> <atom>) form.
  (lambda (pair)
    (let ((itr (car pair))
          (atm (car (cdr pair))))
      (define (inner-explode itr)
        (cond
         ((<= itr 0) '())
         (else (cons atm (inner-explode (- itr 1))))
         ))
      (inner-explode itr))))

(define (decode lst)
  (cond
   ((null? lst) '())
   ((list? (car lst))
    (append
     (explode (car lst))
     (decode (cdr lst))))
   (else (cons (car lst) (decode (cdr lst))))))


;; Coroutines?
(define (count-run lst)
  (define (inside-count lst count)
    (cond
     ((null? lst) '())
     ((not (list? lst)) lst)
     ((eqv? (car lst) (car (cdr lst))) (inside-count (cdr lst) (+ 1 count)))
     (else (cons count (cons (car lst) '())))))
  (inside-count lst 0))

(define (encode-direct lst)
  (let ((current (count-run lst)))
    (cond
     ((null? (cdr lst)) '())
     (else (append current (encode-direct (cdr lst)))))))

(define tests
  (lambda ()
    (print "d -> " (my-last '(a b c d)))
    (print "(f g) -> "(my-but-last '(a b c d e f g)))
    (print "c -> " (element-at '(a b c d e f g) 3))
    (print "5 -> " (count-elements '(a b c d e)))
    (print "(e d c b a) -> " (my-reverse '(a b c d e)))
    (print "#t -> " (my-palindrome? '(a b c d d c b a)))
    (print "#f -> " (my-palindrome? '(a b e b e dg a)))
    (print "#t -> " (my-palindrome? '()))
    (print "(a b c d e f g h) -> " (my-flatten '(a ((b (c d) e f (g) h)))))
    (print "(a b c d e f g h) -> " (rem-dupes '(a a a b b c d d e f f g g g g h h h h h)))
    (print "((a a a) (b b) (c c c) (d) (e e e e e)) -> " (pack-dupes '(a a a b b c c c d e e e e e)))
    (print "((3 a) (2 b) (3 c) (1 d) (5 e)) -> " (encode '(a a a b b c c c d e e e e e)))
    (print "((4 a) b (2 c) (2 a) d (4 e)) -> " (encode-modified '(a a a a b c c a a d e e e e)))
    (print "(a a a b b c c c d e e e e e) -> " (decode '((3 a)(2 b) (3 c) d (5 e))))
    (print "((3 a) (2 b) (3 c) d (5 e)) -> " (encode-direct '(a a a b b c c c d e e e e e)))
    ))

