; ;; TEST STUFF
; (define (double n) (* n 2))
; ;ksdhksjhdsk
; (define (triple n) (* n 3));foo
; triple
; +
; (define foo (lambda (x) (* 2 x)))
; (map double '(1 2 3 4 45))
; foo
; (define (square x) (* x x))
; (define (sum-of-squares x y) (+ (square x) (square y)))
; (define (foo2 a b c)
; 	(sum-of-squares
; 		(if (or (> a b) (> a c)) a b )
; 		(if (or (> c a) (> c b)) c b )
; 	)
; )
; (define ping 40)
; (define pong 60)
; (define (foobar baz qux)
;     (define (bazqux baz qux)
;         (+ baz qux ping pong)
;     )
;     (bazqux baz qux)
; )
; (foobar 1 2)

; ;; END TEST STUFF

; ;; SICP

; (define (sum_ term a next b)
;     (if (> a b)
;         0
;         (+ (term a)
;         (sum_ term (next a) next b))))

; (define (pi-sum a b)
;     (define (pi-term x)
;         (/ 1.0 (* x (+ x 2))))
;     (define (pi-next x)
;         (+ x 4))
;     (sum_ pi-term a pi-next b))

; (define (simpsons f a b n) 
;     (define h (/ (- b a) n))
;     (define (y k)
;         (f (+ a (* k h))))
;     (define (go k acc)
;         (define m (cond
;             [(or (= k n) (= k 0)) 1]
;             [(even? k) 2]
;             [(odd? k) 4]))
;         (if (< k 0)
;             acc
;             (go (- k 1) (+ acc (* m (y k))))))
;     (* (/ h 3) (go n 0)))

; (define (cube x) (* x x x))

; (define (sum-2 term a next b)
;     (define (iter a result)
;     (if (> a b)
;         result
;         (iter (next a) (+ (term a) result))))
;     (iter a 0))

; (define (product_ term a next b)
;     (define (iter a result)
;     (if (> a b)
;         result
;         (iter (next a) (* (term a) result))))
;     (iter a 1))

; (define (id_ n) n)
; (define (inc n) (+ n 1))

; (define (fact n)
;     (product_ id_ 1 inc n))

; (define (wallis n)
;     (define (term n)
;         (* 
;             (/ (* 2 n) (- (* 2 n) 1))
;             (/ (* 2 n) (+ (* 2 n) 1))))

;     (product term 1.0 inc n))

; (define (accumulate combiner null-value term a next b)
;     (define (iter a acc)
;         (if (> a b) acc (iter (next a) (combiner a acc))))
;     (iter a null-value))

; (define (filtered-accumulate combiner filter null-value term a next b)
;     (define (iter a acc)
;         (if (> a b) acc (iter (next a) (if (filter a) (combiner (term a) acc) acc))))
;     (iter a null-value))

; (define (prime? x)
;     (define (prime-helper x k)
;       (cond ((= x k) #t)
;             ((= (remainder x k) 0) #f)
;             (else
;              (prime-helper x (+ k 1)))))
;     (cond ((= x 1) #f)
;           ((= x 2) #t)
;            (else
;             (prime-helper x 2))))
  
; (define (sum-of-squares-of-primes a b)
;     (filtered-accumulate + prime? 0 square a inc b))

; (define tolerance 0.00001)
; (define (fixed-point f first-guess)
;     (define (close-enough? v1 v2)
;         (< (abs (- v1 v2)) 
;         tolerance))
;     (define (try guess)
;         (let ((next (f guess)))
;         (if (close-enough? guess next)
;             next
;             (try next))))
;     (try first-guess))

; (define (average . args) (/ (foldr + 0 args) (length args)))
; (define (sqrt x)
;     (fixed-point 
;      (lambda (y) (average y (/ x y)))
;      1.0))

; (define (golden-ratio)
;     (fixed-point
;         (lambda (y) (average y (+ 1 (/ 1 y))))
;         2.0))

; (define (double_ f) 
;     (lambda (x) (f (f x))))

; (define (nth fn n)
;     (if (= 1 n) 
;         fn
;         (compose fn (nth fn (- n 1)))))

; (define (smooth f dx)
;     (lambda (x) (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

; (define (n-smooth f dx n)
;     (nth (smooth f dx) n))

; (define (f111-rec n)
;     (if (< n 3)
;         3
;         (+ (f111-rec (- n 1)) 
;             (* 2 (f111-rec (- n 2))) 
;             (* 3 (f111-rec (- n 3))))))


; (let* ((a 2) (b (* a 3)) (c (+ b a)))
;     (+ a b c))
; (cons 3 3)
; (define (cons! h t)
;     (lambda (x)
;     (if (= x 0) h t)))
; (define (car! x) (x 0))
; (define (cdr! x) (x 1))

; (define (make-list n)
;     (define (go l n)
;         (if (= n 0) l (go (cons n l) (- n 1))))
;         (go '() n))

; (define (sub1 n) (- n 1))
; (define (zero?_ n) (= 0 n))
; (define x 3)
; x
; (set! x 2)
; x
; (letrec ([is-even? (lambda (n)
;         (or (zero? n)
;             (is-odd? (sub1 n))))]
;         [is-odd? (lambda (n)
;             (and (not (zero? n))
;                 (is-even? (sub1 n))))])
;     (is-odd? 11))

; (define (last-pair ls)
;     (cond
;         [(empty? ls) ls]
;         [(empty? (cdr ls)) (list (car ls))]
;         [else (last-pair (cdr ls))]))

; (define (reverse_ ls)
;     (define (go ls acc)
;         (if (empty? ls) 
;             acc 
;             (go (cdr ls) (cons (car ls) acc))))
;     (go ls '()))

; (define gooo 9)
; (define (blah x)
;     (define gooo 10)
;     (+ gooo x))
; (blah 5)
; (define (mapyyyyyyyy x) (* x 9999999))
; (define (foobarbaz x)
;     (define (double x) (* x 20))
;     (define (triple x) (* x 30))
;     (double (triple x))
; )
; (foobarbaz 11)
; (double (triple 11))

; #(1 2 3 4 5)
; (define vektor #(0 1 2 3 4 5 6 7 8))
; vektor
; (vector-set! vektor 5 69)
; vektor
; (+ 30 30)
; ; (vector-ref vektor 100)
; (vector)
; (vector 1 2 3)
; (make-vector 10 'a)
; ; (vector-map! (lambda (x) (+ x 10)) vektor)
; vektor




(define temp (make-vector 5 'a))
(write "Pre-mutation")
(write temp)
; ; TODO: FIX THIS SHIT
(vector-set! temp 3 9)
(write "Post-mutation")
(write temp)

(define (mut v i val) 
    (write "In mut, pre-mutation")
    (write tempssadasddas)
    (write v)
    (write v)
    (vector-set! v i val) 
    (write "In mut, post-mutation")
    (write temp)
    (write v)
    v)
(define (foo a b) (write a) (write b))
(foo '(1 2 3) 2)
(define temp2 temp)
(define (temper) (let ([temperooni temp] temp) temperooni))
(mut temp 4 6)
(write "Outer, post-mutation")
(write temp)
(write temp2)
; ; temp
; ; (mut temp2 2 69)
; ; temp
; ; (mut (temper) 4 99)
; ; temp
; ; (write vektor)