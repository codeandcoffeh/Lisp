(defun sumrange (select n m)
	(loop 
		for i from n to m
		when (funcall select i) sum i))

(defvar sol 
	(sumrange 
		#'(lambda (x) (or (= 0 (mod x 3)) (= 0 (mod x 5)))) 0 999))