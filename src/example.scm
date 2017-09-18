(load stdlib)

(defn (sort lst)
	(if (null? lst)
		lst
		(let 
			((lesser (filter (curry >= (car lst)) (cdr lst)))
			(greater (filter (curry < (car lst)) (cdr lst))))
			(lst-append* (sort lesser) (list (car lst)) (sort greater)))))

(sort '(3 1 1 2))