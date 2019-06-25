(load (merge-pathnames "dl.lisp" *load-truename*))
(load (merge-pathnames "model.lisp" *load-truename*))
(load "~/quicklisp/setup.lisp")
(ql:quickload "split-sequence")
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
				(print "FAIL: y4 ReLU calculation failed")))))

(defun test-model (input-x-and-y)
	(let ((Y (make-matrix (length (second input-x-and-y)) 1))
		  (X (transpose (make-array (list (length (first input-x-and-y)) (length (first (first input-x-and-y)))) :initial-contents (first input-x-and-y)))))
			  (loop for i from 0 to (- (length (second input-x-and-y)) 1)
			do (setf (aref Y i 0) (nth i (second input-x-and-y))))

		(setq X (mat-divide X 255))
		
		(let ((final_parameters (model 
			X Y 0.01 50)))
			 ; (loop for key being the hash-keys of final_parameters
			    ; using (hash-value value)
			    ; do (format t "final_parameters: The value associated with the key ~S is ~S~%" key value))
		; (format t "~%layer-count: ~S~%" (model-layer-count))
		; (format t "~%predictions: ~S~%" (linear-activation-forward X 
			; (gethash (list 'w (model-layer-count)) final_parameters) 
			; (gethash (list 'b (model-layer-count)) final_parameters) 
			; final_parameters 
			; (layer-function (model-layer-count))))
			
	)))

(defun read-data (path)
	(let ((in (open path))
		  (x-and-y nil))
			(setq x-and-y (read-and-parse-line in))
			(close in)
	x-and-y))

(defun read-and-parse-line (stream)
	(let ((x (list nil))
		  (y (list nil)))
			(when stream
				(loop for line = (read-line stream nil)
					 while line do 
						(let ((parsed (parse-list (split-sequence:SPLIT-SEQUENCE #\, line))))
							(setq y (append y (list (first parsed)))) ; predictions at index 0
							(setq x (append x (list (rest parsed))))  ; input data 
						)))
			(setq y (mapcar (lambda (x) 
						(cond
							((null x) nil)
							((= x 1) 1)
							(t 0))) y))
	(list (rest x) (rest y))))

(defun parse-list (line)
	(mapcar #'parse-integer line))
	
(print '(Test sigmoid))
;(time (test-sigmoid))

(print '(Test ReLU))
;(time (test-relu))

(print '(Test model))
(time (test-model (read-data "mnist_train.csv")))

(print '(Test read data))
;(time (read-data "mnist_train.csv"))