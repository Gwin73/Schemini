(begin
	(def list (lambda lst lst)) ; Defun and L macros not defined

	(def defmacro (macro (lst body) (list 'def (car lst) (list 'macro (cdr lst) body))))

	(defmacro (defun lst body) (list 'def (car lst) (list 'lambda (cdr lst) body))) ;Doesnt work with constant or variadic lambdas
	
	(defmacro (L args body) (list 'lambda args body))
	
	(defun (not x) (if x #f #t))

	(defun (id obj) obj) 

	(defun (flip func) (L (arg1 arg2) (func arg2 arg1))) 

	(defun (comp f g) 
		(L args (f (apply g args)))) 

	(defun (curry func arg1) 
		(L args (apply func (cons arg1 args)))) 

	;List functions
	(defun (null? obj) (if (equal? obj '()) #t #f)) 

	(defun (foldl func acc lst) 
		(if (null? lst)
			acc
			(foldl func (func acc (car lst)) (cdr lst)))) 

	(defun (foldr func acc lst) 
		(if (null? lst)
			acc
			(func (car lst) (foldr func acc (cdr lst))))) 

	(defun (any? pred lst) 
		(foldl (L (acc x) (if (pred x) #t acc)) #f lst)) 

	(defun (every? pred lst)
		(foldl (L (acc x) (if (not (pred x)) #f acc)) #t lst)) 
	
	(defun (member? obj lst) 
		(any? (curry equal? obj) lst)) 

	(defun (lst-append lst1 lst2) 
		(foldr cons lst2 lst1)) 

	(def lst-append* (lambda lst 
		(foldl lst-append '() lst)))

	(defun (lst-length lst) 
		(foldl (L (acc x) (+ acc 1)) 0 lst)) 

	(defun (lst-last lst) 
		(foldl (L (acc x) x) '() lst)) 

	(defun (lst-init lst) 
		(if (= 1 (lst-length lst))
			'()
			(cons (car lst) (lst-init (cdr lst)))))

	(defun (filter pred lst) 
		(foldr (L (x y) (if (pred x) (cons x y) y)) '() lst)) 

	(defun (map func lst)
		(foldr (L (x y) (cons (func x) y)) '() lst)) 

	(defun (reverse lst) 
		(foldl (flip cons) '() lst)) 

	;Bool macros
	(def and (macro lst 
		(if (null? lst)
			'#t
			(list 'if (car lst) (cons 'and (cdr lst)) '#f))))

	(def or (macro lst 
		(if (null? lst)
			'#f
			(list 'if (car lst) #t (cons 'or (cdr lst))))))

	;Int functions
	(defun (odd? n) (= (mod n 2) 1)) 

	(defun (even? n) (= (mod n 2) 0)) 

	(def zero? (curry = 0)) 

    (def pos? (curry < 0)) 

    (def neg? (curry > 0)) 

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

	;Macros
	(def let (macro lst 
		(list 'apply (list 'lambda (map car (lst-init lst)) (lst-last lst)) (list 'quote (map lst-last (lst-init lst))))))

	(def cond (macro lst 
		(if (null? lst)
			#f
			(list 'if (car (car lst)) (lst-last (car lst)) (cons 'cond (cdr lst))))))
)