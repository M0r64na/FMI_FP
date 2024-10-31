(define (my-map f l)
  (if (null? l) '()
      (cons (f (car l)) (my-map f (cdr l)))))

(define (2* x) (* 2 x))

(define (my-filter p l)
  (if (null? l) '()
      (if (p (car l))
          (cons (car l) (my-filter p (cdr l)))
          (my-filter p (cdr l)))))

(define (foldr l op init)
  (if (null? l) init
      (op (car l) (foldr (cdr l) op init))))

(define (sum l)
  (if (null? l) 0
      (+ (car l) (sum (cdr l)))))

(define (forall? p l)
  (if (null? l) #t
      (and (p (car l)) (forall? p (cdr l)))))

(define (exists? p l)
  (if (null? l) #f
      (or (p (car l)) (exists? p (cdr l)))))

(define (my-member2? l x)
  (exists? (lambda (y) (equal? y x)) l))

(define (unique-i l repeated)
  (cond ((null? l) '())
        ((my-member2? repeated (car l)) (unique-i (cdr l) repeated))
        (else (cons (car l) (unique-i (cdr l) (cons (car l) repeated))))))
(define (unique l) (unique-i l '()))

(define (fixpoints-of-some-func-i f xs)
  (cond ((null? xs)'())
        ((equal? (car xs) (f (car xs))) (cons (car xs) (fixpoints-of-some-func-i f (cdr xs))))
        (else (fixpoints-of-some-func-i f (cdr xs)))))
(define (fixpoints-of-some-func-j fs xs)
  (if (null? fs)
      '()
      (append (fixpoints-of-some-func-i (car fs) xs) (fixpoints-of-some-func-j (cdr fs) xs))))
(define (fixpoints-of-some-func fs xs) (unique (fixpoints-of-some-func-j fs xs)))
(define id (lambda (x) x))
(define square (lambda (x) (* x x)))
(define funcs (list id square))
(define args '(0 1 2 3 4 5))

(define (is-bijection-over f items)
  (define (unique? l)
    (if (null? l)
        #t
        (and (not (exists? (lambda (x) (equal? (car l) x)) (cdr l)))
             (unique? (cdr l)))))
  (unique? (my-map f items)))

(define f1 (lambda (x) (+ x 1)))
(define f2 (lambda (x) (if (even? x) x (+ x 1))))
(define items '(1 2 3 4))
(is-bijection-over f1 items)
(is-bijection-over f2 items)
(define items '(1 2 2 4))
(is-bijection-over f1 items)