(load (merge-pathnames "dl.lisp" *load-truename*))
(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))
		 
(defun test-sigmoid ()
	(let (
		(z1 0)
		(z2 12)
		(z3 '(0 12)))
		(let (
			(y1 (sigmoid z1))
			(y2 (sigmoid z2))
			(y3 (sigmoid z3)))
			
			(if (not (equalp 0.5 y1))
				(print "FAIL: y1 sigmoid calculation failed"))
			(if (not (equalp 0.99999386 (float (round-to y2 8 #'floor))))
				(print "FAIL: y2 sigmoid calculation failed"))
			(if (not (equalp 0.5 (first (first y3))))
				(print "FAIL: y3 sigmoid calculation failed"))
			(if (not (equalp 0.99999386 (float (round-to (second (first y3)) 8 #'floor))))
				(print "FAIL: y3 sigmoid calculation failed"))
		)
	)
)

(defun test-relu ()
	(let (
		(x1 -1)
		(x2 0)
		(x3 2.888)
		(x4 '(-1 0 2.888)))
		(let (
			(y1 (relu x1))
			(y2 (relu x2))
			(y3 (relu x3))
			(y4 (relu x4)))
			
			(if (not (equalp 0 y1))
				(print "FAIL: y1 ReLU calculation failed"))
			(if (not (equalp 0 y2))
				(print "FAIL: y2 ReLU calculation failed"))
			(if (not (equalp 2.888 y3))
				(print "FAIL: y3 ReLU calculation failed"))
			(if (not (equalp 0 (first (first y4))))
				(print "FAIL: y4 ReLU calculation failed"))
			(if (not (equalp 0 (second (first y4))))
				(print "FAIL: y4 ReLU calculation failed"))
			(if (not (equalp 2.888 (third (first y4))))
				(print "FAIL: y4 ReLU calculation failed"))))
)

(defun test-model ()
	(let ((*parameters* (model (make-matrix 3 1) '(1 1 0) nil 1)))

		(loop for key being the hash-keys of *parameters*
           using (hash-value value)
           do (format t "The value associated with the key ~S is ~S~%" key value))
	)
)

(print '(Test sigmoid))
;(time (test-sigmoid))

(print '(Test ReLU))
;(time (test-relu))

(print '(Test model))
(time (test-model))