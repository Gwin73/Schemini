;Mixed
(def list (lambda lst lst)) ; defn and fn macros not defined

(def defm (macro (lst body) (list 'def (car lst) (list 'macro (cdr lst) body))))

(defm (defn lst body) (list 'def (car lst) (list 'lambda (cdr lst) body))) ;Doesnt work with constant or variadic lambdas

(defm (fn args body) (list 'lambda args body))

(defn (not x) (if x #f #t))

;Function funtions
(defn (id obj) obj) 

(defn (flip func) (fn (arg1 arg2) (func arg2 arg1))) 

(defn (comp f g) 
	(fn args (f (apply g args)))) 

(defn (curry func arg1) 
	(fn args (apply func (cons arg1 args)))) 

;List functions
(defn (null? obj) (if (equal? obj '()) #t #f)) 

(defn (foldl func acc lst) 
	(if (null? lst)
		acc
		(foldl func (func acc (car lst)) (cdr lst)))) 

(defn (foldr func acc lst) 
	(if (null? lst)
		acc
		(func (car lst) (foldr func acc (cdr lst))))) 

(defn (any? pred lst) 
	(foldl (fn (acc x) (if (pred x) #t acc)) #f lst)) 

(defn (every? pred lst)
	(foldl (fn (acc x) (if (not (pred x)) #f acc)) #t lst)) 

(defn (member? obj lst) 
	(any? (curry equal? obj) lst)) 

(defn (lst-append lst1 lst2) 
	(foldr cons lst2 lst1)) 

(def lst-append* (fn lst 
	(foldl lst-append '() lst)))

(defn (lst-length lst) 
	(foldl (fn (acc x) (+ acc 1)) 0 lst)) 

(defn (lst-last lst) 
	(foldl (fn (acc x) x) '() lst)) 

(defn (filter pred lst) 
	(foldr (fn (x y) (if (pred x) (cons x y) y)) '() lst)) 

(defn (map func lst)
	(foldr (fn (x y) (cons (func x) y)) '() lst)) 

(defn (reverse lst) 
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

;Int functions.
(defn (odd? n) (= (mod n 2) 1)) 

(defn (even? n) (= (mod n 2) 0)) 

(def zero? (curry = 0))

(def pos? (curry < 0)) 

(def neg? (curry > 0)) 

(def sum (fn lst 
		(foldl + 0 lst))) 

(def prod (fn lst 
	(foldl * 1 lst))) 

(def min (fn lst 
	(foldl (fn (acc x) 
		(if (< x acc) x acc)) 
		(car lst) 
		lst)))

(def max (fn lst 
	(foldl (fn (acc x) 
		(if (> x acc) x acc)) 
		(car lst) 
		lst)))

;Macros
(defm (let let-bindings let-body) 
	(list 'apply (list 'fn (map car let-bindings) let-body) (list 'quote (map lst-last let-bindings))))

(def cond (macro lst 
	(if (null? lst)
		#f
		(list 'if (car (car lst)) 
			(lst-last (car lst)) 
			(cons 'cond (cdr lst))))))

(defn (eval lst) 
		(foldr (fn (x y) (lst-append (list 'cons x) (list y))) ''() lst))

(defn (at lst index) 
	(if (= index 0)
		(car lst)
		(at (cdr lst) (- index 1))))

(defn (range n m) 
	(if (= (+ m 1) n)
		'()
		(cons n (range (+ n 1) m))))

(defn (zip lst1 lst2)
	(cond
		((null? lst1) '())
		((null? lst2) '())
		(#t (cons (list (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2))))))

(def t (fn (arg index structI) 
	(let ((gettername (string->atom (str-append (atom->string structI) (str-append "-" (atom->string arg))))))
	(list 'defn (list gettername 'lst)
		(list 'at 'lst index)))))

(defm (defr defr-name defr-argslist)
	(let 
		((defr-predicate (string->atom (str-append (atom->string defr-name) "?")))
		(defr-make (string->atom (str-append (atom->string defr-name) "-make"))))
		(cons 
			'begin 
			(cons 
				(list 'defn (list defr-predicate 'defr-record)
					(list 'equal? (list 'car 'defr-record) (list 'quote defr-name)))
				(cons
					(list 'defn (cons defr-make defr-argslist)
						(list 'cons (list 'quote defr-name) (eval defr-argslist)))
							(map 
								(lambda (defr-pair) (t (car defr-pair) (car (cdr defr-pair)) defr-name))  
								(zip defr-argslist 
									(range 1 (lst-length defr-argslist)))))))))