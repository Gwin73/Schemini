(begin
	;Macros
	(def list (lambda lst lst)) ; Defun and L macros not defined

	(def defmacro (macro (lst body) (list 'def (car lst) (list 'macro (cdr lst) body))))

	(defmacro (defun lst body) (list 'def (car lst) (list 'lambda (cdr lst) body))) ;Doesnt work with constant or variadic lambdas
	
	(defmacro (L args body) (list 'lambda args body))
	;
	(defun (not x) (if x #f #t))

	;Function functions
	(defun (id obj) obj) ;

	(defun (flip func) (L (arg1 arg2) (func arg2 arg1))) ;

	(defun (comp f g) 
		(L args (f (apply g args)))) ;

	(defun (curry func arg1) 
		(L args (apply func (cons arg1 args)))) ;

	;List functions
	(defun (null? obj) (if (equal? obj '()) #t #f)) ;

	(defun (foldl func acc lst) 
		(if (null? lst)
			acc
			(foldl func (func acc (car lst)) (cdr lst)))) ;

	(defun (foldr func acc lst) 
		(if (null? lst)
			acc
			(func (car lst) (foldr func acc (cdr lst))))) ;

	(defun (any? pred lst) 
		(foldl (L (acc x) (if (pred x) #t acc)) #f lst)) ;

	(defun (every? pred lst)
		(foldl (L (acc x) (if (not (pred x)) #f acc)) #t lst)) ;
	
	(defun (member? obj lst) 
		(any? (curry equal? obj) lst)) ;

	(defun (lst-append lst1 lst2) 
		(foldr cons lst2 lst1)) ;

	(defun (lst-length lst) 
		(foldl (L (acc x) (+ acc 1)) 0 lst)) ;

	(defun (lst-last lst) 
		(foldl (L (acc x) x) '() lst)) ;

	(defun (filter pred lst) 
		(foldr (L (x y) (if (pred x) (cons x y) y)) '() lst)) ;

	(defun (map func lst)
		(foldr (L (x y) (cons (func x) y)) '() lst)) ;

	(defun (reverse lst) 
		(foldl (flip cons) '() lst)) ;

	;Bool funtions
	(def and (lambda lst
		(foldl && #t lst)))

	(def or (lambda lst
		(foldl || #f lst)))

	;Int functions
	(defun (odd? n) (= (mod n 2) 1)) ;

	(defun (even? n) (= (mod n 2) 0)) ;

	(def zero? (curry = 0)) ;

    (def pos? (curry < 0)) ;

    (def neg? (curry > 0)) ;

	(def sum (lambda lst 
			(foldl + 0 lst)))

	(def prod (lambda lst 
		(foldl * 1 lst)))

	(def min (lambda lst 
		(foldl (lambda (acc x) 
			(if (< x acc) x acc)) 
			(car lst) 
			lst)))

	(def max (lambda lst 
		(foldl (lambda (acc x) 
			(if (> x acc) x acc)) 
			(car lst) 
			lst)))
)