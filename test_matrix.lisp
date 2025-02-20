(load (merge-pathnames "matrix.lisp" *load-truename*))

(defun test-transpose ()
	(let (
		(x 2)
		(m 2))
		(let (
			(matrix (make-matrix x m)))
			(print matrix)
			(print 'transposed)
			(print (transpose matrix))
			(if (not (equalp (transpose (transpose matrix)) matrix))
				(print "FAIL: matrix transpose failed"))
			
			(if (not (equalp (aref matrix 0 1) (aref (transpose matrix) 1 0)))
				(print "FAIL: matrix transpose failed"))
)))


(defun test-multiplication ()
	(let (
		(x 3)
		(m 3))
		(let (
			(matrix (make-matrix x m))
			(matrix2 (make-matrix m x)))
			;(print matrix)
			;(print matrix2)
			(let ((multiplied (matmul matrix matrix2)))
				(if (eql 'dimensions-no-match multiplied)
					(print "FAIL: matrix dimensions do not match"))
				(print 'multiplied)
				(print multiplied)
				
				(unless (equalp
					(aref multiplied 0 0)
					
					(+ (* (aref matrix 0 0) (aref matrix2 0 0))(* (aref matrix 0 1) (aref matrix2 1 0)) (* (aref matrix 0 2) (aref matrix2 2 0))))
					(print "FAIL: multiplied index (0,0) calculated incorrecty"))
				
				(unless (equalp 
					(aref multiplied 0 1) 
					(+ (* (aref matrix 0 0) (aref matrix2 0 1))(* (aref matrix 0 1) (aref matrix2 1 1)) (* (aref matrix 0 2) (aref matrix2 2 1))))
					(print "FAIL: multiplied index (0,1) calculated incorrecty"))
					
				(unless (equalp 
					(aref multiplied 1 0) 
					(+ (* (aref matrix 1 0) (aref matrix2 0 0))(* (aref matrix 1 1) (aref matrix2 1 0)) (* (aref matrix 1 2) (aref matrix2 2 0))))
					(print "FAIL: multiplied index (1,0) calculated incorrecty"))
				
				(unless (equalp 
					(aref multiplied 1 1) 
					(+ (* (aref matrix 1 0) (aref matrix2 0 1))(* (aref matrix 1 1) (aref matrix2 1 1)) (* (aref matrix 1 2) (aref matrix2 2 1))))
					(print "FAIL: multiplied index (1,1) calculated incorrecty"))
					
				(unless (and (eql (columncount multiplied) (columncount matrix2)) (eql (rowcount multiplied) (rowcount matrix)))
					(print "FAIL: multiplied matrix dimensions not correct"))
			)
		)
	)
)

(defun test-dot ()
	(let (
		(x 2)
		(m 2))
		(let (
			(matrix (make-matrix x m))
			(matrix2 (make-matrix m x)))
			;(print matrix)
			;(print matrix2)
			(setf (aref matrix 0 0) 1)
			(setf (aref matrix 0 1) 0)
			(setf (aref matrix 1 0) 0)
			(setf (aref matrix 1 1) 1)
			(setf (aref matrix2 0 0) 4)
			(setf (aref matrix2 0 1) 1)
			(setf (aref matrix2 1 0) 2)
			(setf (aref matrix2 1 1) 2)
			(let ((multiplied (matmul matrix matrix2)))
				(if (eql 'dimensions-no-match multiplied)
					(print "FAIL: matrix dimensions do not match"))
				(print 'multiplied)
				(print multiplied)
				
				(unless (equalp (aref multiplied 0 0) 4.0)
					(print "FAIL: multiplied index (0,0) calculated incorrecty"))
				(unless (equalp (aref multiplied 0 1) 1.0)
					(print "FAIL: multiplied index (0,1) calculated incorrecty"))
				(unless (equalp (aref multiplied 1 0) 2.0)
					(print "FAIL: multiplied index (1,0) calculated incorrecty"))
				(unless (equalp (aref multiplied 1 1) 2.0)
					(print "FAIL: multiplied index (1,1) calculated incorrecty"))
			)
		)
	)
)

(defun test-matadd ()
	(let (
		(x 2)
		(m 2))
		(let (
			(matrix1 (make-matrix x m)))
			(print matrix1)
			;(print matrix2)
			(setf (aref matrix1 0 0) 1.1)
			(setf (aref matrix1 0 1) 2.2)
			(setf (aref matrix1 1 0) 3.3)
			(setf (aref matrix1 1 1) 4.4)
			(let ((added (mat-add matrix1 2.5))
				  (added2 (mat-add matrix1 '(1.0 2.0))))
				(print 'added)
				(print added)
				(print added2)
				
				(unless (equalp (aref added 0 0) 3.6)
					(print "FAIL: added index (0,0) calculated incorrecty"))
				(unless (equalp (aref added 0 1) 4.7)
					(print "FAIL: added index (0,1) calculated incorrecty"))
				(unless (equalp (aref added 1 0) 5.8)
					(print "FAIL: added index (1,0) calculated incorrecty"))
				(unless (equalp (aref added 1 1) 6.9)
					(print "FAIL: added index (1,1) calculated incorrecty"))

				(unless (equalp (aref added2 0 0) 2.1)
					(print "FAIL: added index (0,0) calculated incorrecty"))
				(unless (equalp (aref added2 0 1) 3.2)
					(print "FAIL: added index (0,1) calculated incorrecty"))
				(unless (equalp (aref added2 1 0) 5.3)
					(print "FAIL: added index (1,0) calculated incorrecty"))
				(unless (equalp (aref added2 1 1) 6.4)
					(print "FAIL: added index (1,1) calculated incorrecty"))
			)
		)
	))

(print '(Test transpose))
(time (test-transpose))
(print '(Test multiplication))
(time (test-multiplication))
(print '(Test dot))
(time (test-dot))
(print '(Test matadd))
(time (test-matadd))