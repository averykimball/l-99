(use extras)

;; P1-P28 Lists

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
      (my-reverse-2 (cdr list) (cons (car list) acc)))))
  (my-reverse-2 list '()))

(define (my-palindrome? list) ; !
  (define (compare? list rev)
    (cond
     ((null? list) #t)
     ((not (eqv? (car list) (car rev))) #f)
     (else (compare? (cdr list) (cdr rev)))))
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
     (else (cons (car lst) (inside-rem (cdr lst) (car lst))))))
  (inside-rem lst #f))

(define (pack-dupes lst) ; ! Note: Last thing you do (pack the last acc, in this case) must go in base!
  (define (inside-pack lst last acc)
    (cond
     ((null? lst) (cons acc '()))
     ((eqv? (car lst) last) (inside-pack (cdr lst) (car lst) (cons (car lst) acc)))
     (else (cons acc (inside-pack (cdr lst) (car lst) (cons (car lst) '()))))))
  (inside-pack lst (car lst) '()))

(define (encode lst) ; ! Improvement: Mod algorithm so there's no need to call my-reverse (maybe no accum)
  (let ((packed (my-reverse (pack-dupes lst))))
    (define (inner-encode lst acc)
      (cond
       ((null? lst) acc)
       (else
        (inner-encode (cdr lst)
                      (cons
                       (cons (count-elements (car lst)) (cons (car (car lst)) '()))
                       acc)))))
    (inner-encode packed '())))

(define (encode-modified lst)
  (let ((packed (my-reverse (pack-dupes lst))))
    (define (inner-encode lst acc)
      (cond
       ((null? lst) acc)
       ((eq? 1 (count-elements (car lst))) (inner-encode (cdr lst) (cons (car (car lst)) acc)))
       (else 
        (inner-encode (cdr lst)
                      (cons
                       (cons (count-elements (car lst)) (cons (car (car lst)) '()))
                       acc)))))
    (inner-encode packed '())))

(define explode ; Takes a list (not dotted pair) of '(<fixnum> <atom>) form.
  (lambda (pair)
    (let ((itr (car pair))
          (atm (car (cdr pair))))
      (define (inner-explode itr)
        (cond
         ((<= itr 0) '())
         (else (cons atm (inner-explode (- itr 1))))))
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
     ((not (list? (cdr lst))) lst)
     ((eqv? (car lst) (car (cdr lst))) (inside-count (cdr lst) (+ 1 count)))
     (else (cons count (cons (car lst) '())))))
  (inside-count lst 0))

;; CURRENT: Implement run-length encoding, don't explicitly create the sublists, only count them. Also, replace singleton lists with the char.

(define (encode-direct lst) ; Based off rem-dupes
  (define (inside-direct lst last count)
    (cond
     ((null? lst)
      (cons (cons count (cons last '())) '()))
     ((and (not (eqv? (car lst) last))
           (eqv? count 1))
      (cons last (inside-direct lst (car lst) 0)))
     ((eqv? (car lst) last) (inside-direct (cdr lst) last (+ 1 count)))
     (else (cons (cons count (cons last '())) (inside-direct lst (car lst) 0)))))
  (inside-direct lst (car lst) 0))

(define (dupli lst)
  (cond
   ((null? lst) '())
   (else (append
          (cons (car lst) (cons (car lst) '()))
          (dupli (cdr lst))))))

(define (repli lst count)
  (define (inner-repli el count)
    (cond
     ((eqv? count 0) '())
     (else (cons el (inner-repli el (- count 1))))))
  (cond
   ((null? lst) '())
   (else (append (inner-repli (car lst) count) (repli (cdr lst) count)))))

(define (drop lst outer-count) ;; Use a let?
  (define (inner-drop lst count)
    (cond
     ((null? lst) '())
     ((eqv? count 1) (inner-drop (cdr lst) outer-count))
     (else (cons (car lst) (inner-drop (cdr lst) (- count 1))))
     ))
  (inner-drop lst outer-count))

(define (split lst place)
  (define (build-first lst place)
    (cond ((eqv? place 0) '())
          (else (cons (car lst) (build-first (cdr lst) (- place 1))))))
  (define (build-second lst place)
    (cond ((null? lst)'())
          ((eqv? place 0) (cons (car lst) (build-second (cdr lst) 0)))
          (else (build-second (cdr lst) (- place 1)))))
  (cons (build-first lst place) (cons (build-second lst place) '())))

(define (slice lst begin end)
  (define (skip-start lst begin)
    (cond
     ((eqv? begin 1) lst)
     (else (skip-start (cdr lst) (- begin 1)))))
  (define (get-range lst end)
    (cond
     ((eqv? end 0) '())
     (else (cons (car lst) (get-range (cdr lst) (- end 1))))))
  (get-range (skip-start lst begin) (+ (- end begin) 1)))


(define (rotate lst places)
  (define (rotate-left lst places)
    (cond ((eqv? places 0) lst)
          (else (append (car (cdr (split lst places))) (car (split lst places))))))
  (define (rotate-right lst places)
    (append
     (car (cdr (split lst (+ (length lst) places))))
     (car (split lst (+ (length lst) places)))))
  (cond
   ((> 0 places) (rotate-right lst places))
   (else (rotate-left lst places))))

(define (remove-at lst pos)
  (cond
   ((null? lst) '())
   ((<= pos 0) '())
   ((= pos 1) (append (cdr lst) '()))
   (else
    (append
     (car (split lst (- pos 1))) (car (cdr (split lst pos)))))))

(define (insert-at atom lst place)
  (append
   (car (split lst (- place 1))) (cons atom '()) (car (cdr (split lst (- place 1))))))

(define (range begin end)
  (define (range-up begin end)
    (define (inner begin current)
      (cond
       ((eqv? current end) (cons current '()))
       (else (cons current (inner begin (+ current 1))))))
    (inner begin begin))
  (define (range-down end begin)
    (define (inner end current)
      (cond
       ((eqv? current begin)
        (cons current '()))
       (else (cons current (inner end (- current 1))))))
    (inner end end))
  (cond
   ((> begin end) (range-down begin end))
   (else (range-up begin end))))

(define (rnd-select lst count)
  (let ((rando (+ 1 (random (- (length lst) 1)))))
    (cond
     ((eqv? count 0) '())
     (else (cons (element-at lst rando) (rnd-select (remove-at lst rando) (- count 1)))))))

(define (lotto-select count limit)
  (rnd-select (range 0 limit) count))

(define (rnd-permu lst)
  (let* ((rando (+ 1 (random (- (length lst) 1)))))
     (cond
      ((null? lst) '())
      (else (cons (element-at lst rando) (rnd-permu (remove-at lst rando))))
      )))

;; (define (combination count pool)
;;   (cond
;;    ((> (length pool) count) '())
;;    ((= (length pool) count) pool) ;; Base-case termination
;;    (else
;;     (cons ())
;;     )
;;    )  
;;   )

;; (define (combination count pool)
;;   (define (build-committee subcount subpool)
;;     (cond
;;      ((=< subcount 0) '())
;;      (else
;;       (cons (build-committee (- subcount 1) (cdr subpool)))
;;       )
;;      )
;;     )
;;   (define (set-build pool)
;;     (cond
;;      ((> (length pool) count) '())
;;      ((= (length pool) count) pool)
;;      (else '())
;;      )
;;     )
;;   )

(define (trivial count pool)
  (cond ((= count 0) '())
        (else (cons (car pool) (trivial (- count 1) (cdr pool))))
        ))

(define (compli-trivial count current rest)
  (cond ((= count 0) '())
        (else (cons current (compli-trivial (- count 1) )))
        ))

(print (trivial 3 '(a b c d e f g)))


(define (build-arb count pool)
  (cond
   ((= count 0) '())
   ((null? pool) '())
   (else (cons (cons (car pool) (build-arb (- count 1) (cdr pool)))
               (build-arb count (cdr pool))
               ))))





;; P54-P69 Binary Trees

;; P70-P73 Multiway Trees

;; P80-89 Graphs

(define tests
  (lambda ()
    ;; 01
    (print "d -> " (my-last '(a b c d)))
    ;; 02
    (print "(f g) -> "(my-but-last '(a b c d e f g)))
    ;; 03
    (print "c -> " (element-at '(a b c d e f g) 3))
    ;; 04
    (print "5 -> " (count-elements '(a b c d e)))
    ;; 05
    (print "(e d c b a) -> " (my-reverse '(a b c d e)))
    ;; 06
    (print "#t -> " (my-palindrome? '(a b c d d c b a)))
    (print "#f -> " (my-palindrome? '(a b e b e dg a)))
    (print "#t -> " (my-palindrome? '()))
    ;; 07
    (print "(a b c d e f g h) -> " (my-flatten '(a ((b (c d) e f (g) h)))))
    ;; 08
    (print "(a b c d e f g h) -> " (rem-dupes '(a a a b b c d d e f f g g g g h h h h h)))
    ;; 09
    (print "((a a a) (b b) (c c c) (d) (e e e e e)) -> " (pack-dupes '(a a a b b c c c d e e e e e)))
    ;; 10
    (print "((3 a) (2 b) (3 c) (1 d) (5 e)) -> " (encode '(a a a b b c c c d e e e e e)))
    ;; 11
    (print "((4 a) b (2 c) (2 a) d (4 e)) -> " (encode-modified '(a a a a b c c a a d e e e e)))
    ;; 12
    (print "(a a a b b c c c d e e e e e) -> " (decode '((3 a)(2 b) (3 c) d (5 e))))
    ;; 13
    (print "((3 a) (2 b) (3 c) d (5 e)) -> " (encode-direct '(a a a b b c c c d e e e e e)))
    ;; 14
    (print "(b b c c a a a a a a f f) -> " (dupli '(b c a a a f)))
    ;; 15
    (print "(b b b c c c a a a e e e) -> " (repli '(b c a e) 3))
    ;; 16
    (print "(a b d e g h k) -> " (drop '(a b c d e f g h i k) 3))
    ;; 17
    (print "((a b c) (d e f g h i k)) -> " (split '(a b c d e f g h i k) 3))
    ;; 18
    (print "(c d e f g) -> " (slice '(a b c d e f g h i k l m n o p) 3 7))
    ;; 19
    (print "(d e f g h a b c) -> " (rotate '(a b c d e f g h) 3))
    (print "(g h a b c d e f) -> " (rotate '(a b c d e f g h) -2))
    ;; 20
    (print "(a c d) -> " (remove-at '(a b c d) 2))
    ;; 21
    (print "(a alfa b c d) -> " (insert-at 'alfa '(a b c d) 2))
    ;; 22
    (print "(4 5 6 7 8 9) -> " (range 4 9))
    (print "(9 8 7 6 5 4) -> " (range 9 4))
    ;; 23
    (print "(*three random atoms*) -> " (rnd-select '(a b c d e f g h i k) 3))
    ;; 24
    (print "(*six random numbers*) -> " (lotto-select 6 50))
    ;; 25
    (print "(*shuffled atoms*) -> " (rnd-permu '(a b c d e f g h i k)))
    ))
