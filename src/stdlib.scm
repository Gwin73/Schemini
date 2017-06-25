(begin
	(def not (\ (x) 
		(if x #f #t)))

	;Function functions
	(def id (\ (obj) obj))

	(def flip (\ (func)
		(\ (arg1 arg2) (func arg2 arg1))))

	(def comp (\ (f g) 
		(\ args (f (apply g args)))))

	(def curry (\ (func arg1) 
		(\ args (apply func (cons arg1 args)))))

	;List functions
	(def null? (\ (obj) 
		(if (equal? obj '()) #t #f)))

	(def foldl (\ (func acc lst) 
		(if (null? lst)
			acc
			(foldl func (func acc (car lst)) (cdr lst)))))

	(def foldr (\ (func acc lst) 
		(if (null? lst)
			acc
			(func (car lst) (foldr func acc (cdr lst))))))

	(def any? (\ (pred lst) 
		(foldl (\ (acc x) (if (pred x) #t acc)) #f lst)))

	(def every? (\ (pred lst) 
		(foldl (\ (acc x) (if (not (pred x)) #f acc)) #t lst)))
	
	(def member? (\ (obj lst)
		(any? (curry equal? obj) lst)))

	(def lst-append (\ (lst1 lst2) 
		(foldr cons lst2 lst1)))

	(def lst-length (\ (lst) 
		(foldl (\ (acc x) (+ acc 1)) 0 lst)))

	(def last (\ (lst) 
		(foldl (\ (acc x) x) '() lst)))

	(def filter (\ (pred lst) 
		(foldr (\ (x y) (if (pred x) (cons x y) y)) '() lst)))

	(def map(\ (func lst)
		(foldr (\ (x y) (cons (func x) y)) '() lst)))

	(def reverse (\ (lst) (foldl (flip cons) '() lst)))

	;Bool funtions
	(def and (\ lst
		(foldl && #t lst)))

	(def or (\ lst
		(foldl || #f lst)))

	;Int functions
	(def odd? (\ (n) 
		(= (mod n 2) 1)))

	(def even? (\ (n)
		(= (mod n 2) 0)))

	(def zero? (curry = 0))

    (def pos? (curry < 0))

    (def neg? (curry > 0))

	(def sum (\ lst 
			(foldl + 0 lst)))

	(def prod (\ lst 
		(foldl * 1 lst)))

	(def min (\ lst 
		(foldl (\ (acc x) 
			(if (< x acc) x acc)) 
			(car lst) 
			lst)))

	(def max (\ lst 
		(foldl (\ (acc x) 
			(if (> x acc) x acc)) 
			(car lst) 
			lst)))
)