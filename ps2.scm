
;; Created by Yilun Qian
;; Student ID: 301243658

;;Question 1
(define double-the-cheese
	(lambda (lst)
		(cond 
			((null? lst)
				'())
			((equal? 'cheese (car lst))
				(cons 'cheese (cons 'cheese (double-the-cheese (cdr lst)))))
			(else
				(cons (car lst) (double-the-cheese(cdr lst))))
		)
	)
)

;;Question 2
(define my-last
	(lambda (lst)
		(cond
			((null? lst)
				(error "empty list"))
			((equal? 1 (length lst))
				(car lst))
			(else
				(my-last(cdr lst)))
		)
	)
)

;;Question 3
(define deep-sum
	(lambda (lst)
		(cond
			((null? lst)
				0)
			((number? (car lst))
				(+ (car lst) (deep-sum(cdr lst))))
			((list? (car lst))
				(+ (deep-sum(car lst)) (deep-sum(cdr lst))))
			(else
				(deep-sum(cdr lst)))
		)
	)
)

;;Question 4
(define is-bit?
	(lambda (x)
		(cond
			((equal? 1 x)
				#t)
			((equal? 0 x)
				#t)
			(else
				#f)
		)
	)
)

;;Question 5
(define is-bit-seq?
	(lambda (lst)
		(cond
			((null? lst)
				'true)
			((is-bit? (car lst))
				(is-bit-seq? (cdr lst)))
			(else
				'false)
		)
	)
)

;;Question 6
(define add-zeroes
	(lambda (x)
		(cond
			((null? x)
				(list 0))
			((null? (car x))
				(cons (list 0) (list x))
			)
			((null? (cdr x))
				(cons (cons '0 (list (car x))) '()))
			(else
				(cons (cons '0 (list (car x))) (add-zeroes (cdr x))))
		)
	)
)

(define add-ones
	(lambda (x)
		(cond
			((null? x)
				(list 1))

			((null? (car x))
				(cons '(1) (list x))
			)
			((null? (cdr x))
				(cons (cons '1 (list (car x))) '()))
			(else
				(cons (cons '1 (list (car x))) (add-ones (cdr x))))
		)
	)
)

(define combine
	(lambda (x y)
		(cond 
			((null? x)
				y)
			(else
				(combine (cdr x) (cons (car x) y)))
		)
	)
)

(define all-bit-seqs
	(lambda (n)
		(cond
			((< n 1)
				'())
			(else
				(combine (add-zeroes (all-bit-seqs (- n 1)))
					(add-ones (all-bit-seqs (- n 1)))))
		)
	)
)

;;Question 7
(define range
	(lambda (n)
		(cond
			((positive? n)
				(append (range (- n 1)) (list (- n 1))))
			(else 
				'())
		)
	)
)

;;Question 8
;; faster running time
(define is-prime?
	(lambda (n d)
		(cond
			((equal? 1 n)
				#f)
			((and (equal? 2 n) (equal? 2 d))
				#t)
			((equal? n d)
				#t)
			((> d (sqrt n))
				'Done!)
			(else
				(cond 
					((equal? 0 (modulo n d))
						#f)
					(else
						(is-prime? n (+ d 1)))
				)
			)
		)
	)
)

(define count-primes
	(lambda (n)
		(cond
			((not (positive? n))
				0)
			(else
				(cond 
					((is-prime? n 2)
						(+ 1 (count-primes(- n 1))))
					(else
						(count-primes (- n 1)))
				)
			)
		)
	)
)


