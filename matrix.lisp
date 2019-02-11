(defun is-matrix (a)
	(typep a 'array))

(defun init-matrix (matrix &optional default)
  "Initialize matrix with random numbers"
  (let ((m (rowcount matrix))
		(n (columncount matrix)))
			(loop for x from 0 to (- m 1)
				do (loop for y from 0 to (- n 1)
					do (cond((eq default nil) (setf (aref matrix x y) (random 1.0)))
						(t (setf (aref matrix x y) default)))))
	  matrix))

(defun make-matrix (m n &optional default)
  "Make array with dimensions m rows, n columns"
  (init-matrix (make-array (list m n)) default))

(defun rowcount (matrix)
  "Gets row count of matrix"
  (array-dimension matrix 0))

(defun columncount (matrix)
  "Gets column count of matrix"
  (array-dimension matrix 1))

(defun dot (a b)
  "Matrix a and list b multiplication"
  (print 'dot)
  (print a)
  (print b)
  (assert (eql (columncount a) (length b))
            (a b) 
            "Cannot multiply matrices, dimensions don't match")
  (let  ((row (rowcount a))
		 (column (columncount b))
		 (tmp (make-matrix (rowcount a) 1 0.0)))
			(loop for x from 0 to (- row 1)
				do (loop for y from 0 to (- column 1)
					do (setf (aref tmp x 0) (dot-row-and-column a b x y))))
	tmp))

(defun dot-row-and-column (a b rownum columnnum)
	(let ((sum 0.0))
		(loop for x from 0 to (- (columncount a) 1)
			do (setf sum (+ sum (* (aref a rownum x) (nth x b)))))
	
	sum))
	
(defun add-row (row)
	(let ((sum 0.0))
		(loop for x from 0 to (- (columncount row) 1)
			do (setf sum (+ sum (aref row 0 x) )))
	
	sum))
	
(defun add-column (column)
	(let ((sum 0.0))
		(loop for x from 0 to (- (rowcount column) 1)
			do (setf sum (+ sum (aref column x 0) )))
	
	sum))
	
(defun matmul (a b)
  "Matrix multiplication"
  ;(print 'matmul)
  ;(print a)
  ;(print b)
  (assert (eql (columncount a) (rowcount b))
            (a b) 
            "Cannot multiply matrices, dimensions don't match")
  (let ((row (rowcount a))
				 (column (columncount b))
				 (tmp (make-matrix (rowcount a) (columncount b) 0.0)))
					(loop for x from 0 to (- row 1)
						do (loop for y from 0 to (- column 1)
							do (setf (aref tmp x y) (multiply-row-and-column a b x y))))
			tmp))

(defun multiply-row-and-column (a b rownum columnnum)
	(let ((sum 0.0))
		(loop for x from 0 to (- (columncount a) 1)
			do (setf sum (+ sum (* (aref a rownum x) (aref b x columnnum)))))
	
	sum))
			
(defun transpose (matrix)
  "Transpose matrix, non-destructive"
  (let ((tmp (make-matrix (columncount matrix) (rowcount matrix) 0.0)))
		(loop for x from 0 to (- (rowcount matrix) 1)
			do (loop for y from 0 to (- (columncount matrix) 1)
				do (
					setf (aref tmp y x) (aref matrix x y))))
		tmp))

(defun mat-add (matrix b)
	(matrix-apply matrix b #'+))
	
(defun mat-minus (matrix b)
	(matrix-apply matrix b #'-))
	
(defun mat-multiply (matrix b)
	(matrix-apply matrix b #'*))
	
(defun matrix-apply (matrix b function)
	"Add scalar or list of scalars to matrix"
	;(print 'matadd)
	(assert (or (not (listp b)) (eql (length b) (rowcount matrix)))
            (matrix b) 
            "Cannot matadd, dimensions don't match")
	
	(cond 
		((and (listp b) (not (eql (length b) (rowcount matrix))))
			'dimensions-no-match)
		((and (is-matrix b) (not (eql (rowcount b) (rowcount matrix))))
			'dimensions-no-match)
		((listp b) 	
			(let ((tmp (make-matrix (rowcount matrix) (columncount matrix) 0.0)))
				(loop for x from 0 to (- (rowcount matrix) 1)
					do (loop for y from 0 to (- (columncount matrix) 1)
						do (setf (aref tmp x y) (funcall function (aref matrix x y) (nth x b)))))
			tmp))
		((is-matrix b) 	
			(let ((tmp (make-matrix (rowcount matrix) (columncount matrix) 0.0)))
				(loop for x from 0 to (- (rowcount matrix) 1)
					do (loop for y from 0 to (- (columncount matrix) 1)
						
						do (setf (aref tmp x y) (funcall function (aref matrix x y) (aref b x 0)))))
			tmp))
		(t 		
			(let ((tmp (make-matrix (rowcount matrix) (columncount matrix) 0.0)))
				(loop for x from 0 to (- (rowcount matrix) 1)
					do (loop for y from 0 to (- (columncount matrix) 1)
						do (setf (aref tmp x y) (funcall function (aref matrix x y) b))))
			tmp))))
			
(defun matrix-map (function matrix
                  &optional (retval (make-array (array-dimensions matrix))))
	"Apply FUNCTION to each element of ARRAY.
	Return a new array, or write into the optional 3rd argument."
	(dotimes (i (array-total-size matrix) retval)
		(setf (row-major-aref retval i)
			  (funcall function (row-major-aref matrix i)))))
			  
(defun element-wise-multiply (A B) (element-wise-matrix #'* A B))

; https://rosettacode.org/wiki/Element-wise_operations#Common_Lisp
(defun element-wise-matrix (fn A B)
  (let* ((len (array-total-size A))
         (m   (car (array-dimensions A)))
         (n   (cadr (array-dimensions A)))
         (C   (make-array `(,m ,n) :initial-element 0.0d0)))
 
    (loop for i from 0 to (1- len) do
         (setf (row-major-aref C i) 
               (funcall fn
                        (row-major-aref A i)
                        (row-major-aref B i))))
    C))