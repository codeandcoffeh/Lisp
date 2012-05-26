(defun ultimate (ls)
	(kth (1- (length ls)) ls))

(defun penultimate (ls)
	(kth (- 2 (length ls)) ls))

(defun kth (n ls)
	(let ((c 0))
		(dolist (i ls)
			(when (= c n)
				(return-from kth i))
			(incf c))))

(defun extent (ls)
	(reduce #'+ ls :key (lambda (x) 1)))

(defun flip (ls)
	(let ((res nil))
		(dolist (i ls)
			(push i res))
		res))

(defun palindrome-p (ls)
	(equal ls (flip ls)))

(defun flatten (ls)
	(cond ((null ls) nil)
		  ((atom (car ls))
		  	(append (list (car ls)) (flatten (cdr ls))))
		  ((listp (car ls))
		  	(append (flatten (car ls)) (flatten (cdr ls))))))

(defun delete-consecutive (ls)
	(let ((prev nil) (res nil))
		(dolist (i ls)
			(when (not (equal i prev))
				(push i res)
				(setf prev i)))
		(reverse res)))

(defun pack (ls)
	(let ((prev (first ls)) (temp nil) (res nil))
		(dolist (i ls)
			(if (equal i prev)
				(push i temp)
				(progn
					(push temp res)
					(setf temp nil)
					(push i temp)
					(setf prev i))))
		(push temp res)
		(reverse res)))

(defun encode (ls)
	(mapcar (lambda (x) (list (length x) (first x))) (pack ls)))

(defun encode-mod (ls)
	(mapcar (lambda (x) (let ((len (length x))) 
							(if (> len 1)
								(list len (first x))
								(first x)))) (pack ls)))

(defun replicate (n ls)
	(let ((res nil))
		(dotimes (i n)
			(if (atom ls)
				(push ls res)
				(dolist (e ls)
					(push e res))))
		(cond ((equal (class-name (class-of ls)) 'string)
			  	(reduce (lambda (acc x) (concatenate 'string acc x)) (reverse res)))
			  ((equal (class-name (class-of ls)) 'character)
			  	(coerce (reverse res) 'string))
			  (t
			  	(reverse res)))))

(defun decode (ls)
	(flatten (mapcar (lambda (x) (if (not (listp x))
									x
									(replicate (first x) (second x)))) ls)))

(defun dupli (ls &optional (n 2))
	(flatten (mapcar (lambda (x) (replicate n x)) ls)))

(defun drop-modulus (n ls)
	(let ((c 0) (res nil))
		(dolist (i ls)
			(incf c)
			(when (not (zerop (mod c n)))
				(push i res)))
		(reverse res)))







