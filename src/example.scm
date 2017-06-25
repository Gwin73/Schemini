(begin
	(load stdlib)

	(def sort (\ (lst)
		(if (null? lst)
			lst
			(lst-append 
				(lst-append 
					(sort (filter (curry >= (car lst)) (cdr lst))) 
					(cons (car lst) '())) 
				(sort (filter (curry < (car lst)) (cdr lst)))))))

	(sort '(3 1 1 2))
)