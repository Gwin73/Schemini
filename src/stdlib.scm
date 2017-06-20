(begin
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
		(any? (\ (x) (equal? obj x)) lst)))

	(def lst-append (\ (lst1 lst2) 
		(foldr cons lst2 lst1)))

	(def lst-length (\ (lst) 
		(foldl (\ (acc x) (+ acc 1)) 0 lst)))

	(def last (\ (lst) 
		(foldl (\ (acc x) x) '() lst)))

	(def filter (\ (pred lst) 
		(foldr (\ (x y) (if (pred x) (cons x y) y)) '() lst)))

	;Bool funtions
	(def not (\ (x) 
		(if x #f #t)))

	(def and (\ lst
		(foldl && #t lst)))

	(def or (\ lst
		(foldl || #f lst)))

	;Function functions
	(def id (\ (obj) obj))

	;Int functions
	(def odd? (\ (n) 
		(= (mod n 2) 1)))

	(def even? (\ (n)
		(= (mod n 2) 0)))

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