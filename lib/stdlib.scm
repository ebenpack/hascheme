(define (not x) (if x #f #t))

(define (list . objs) objs)

(define (id obj) obj)

(define (compose a b) (lambda (x) (a (b x))))

(define (flip fn) (lambda (x y) (fn y x)))

(define (curry func arg1)  (lambda (arg) (apply func (cons arg1 (list arg)))))

(define zero? (curry = 0))

(define positive? (curry < 0))

(define negative? (curry > 0))

(define (odd? num) (= (modulo num 2) 1))

(define (even? num) (= (modulo num 2) 0))

(define (abs n) (if (< n 0) (- 0 n) n))

(define (foldl fn acc ls)
    (if (empty? ls)
        acc
        (foldl fn (fn acc (car ls)) (cdr ls))))

(define (foldr fn acc ls)
    (if (empty? ls)
        acc
        (fn (car ls) (foldr fn acc (cdr ls)))))
(define (unfold func init pred)
    (if (pred init)
        (cons init '())
        (cons init (unfold func (func init) pred))))

(define fold foldl)

(define reduce foldr)

(define (sum . lst) (fold + 0 lst))

(define (product . lst) (fold * 1 lst))

(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))

(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))

(define (length lst) (fold (lambda (x y) (+ x 1)) 0 lst))

(define (reverse lst) (fold (flip cons) '() lst))

(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))

(define (memq obj lst) (fold (mem-helper (curry eq? obj) id) #f lst))

(define (memv obj lst) (fold (mem-helper (curry eqv? obj) id) #f lst))

(define (member obj lst) (fold (mem-helper (curry equal? obj) id) #f lst))

(define (assq obj alist) (fold (mem-helper (curry eq? obj) car) #f alist))

(define (assv obj alist) (fold (mem-helper (curry eqv? obj) car) #f alist))

(define (assoc obj alist) (fold (mem-helper (curry equal? obj) car) #f alist))

(define (map fn ls)
    (foldr (lambda (x y) (cons (fn x) y)) '() ls))

(define (filter pred ls)
    (foldr 
        (lambda (x y)
            (if (pred x)
                (cons x y) 
                y))
    '() ls))