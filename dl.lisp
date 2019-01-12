(load (merge-pathnames "matrix.lisp" *load-truename*))
(load (merge-pathnames "model.lisp" *load-truename*))
(defconstant e (exp 1))

(defun sigmoid (z)
	"Computes the sigmoid of matrix, scalar, or list z"
	(cond 
		((is-matrix z)
			(let ((tmp (make-matrix (rowcount z) (columncount z) 0.0)))
				(loop for x from 0 to (- (rowcount z) 1)
					do (loop for y from 0 to (- (columncount z) 1)
						do (setf (aref tmp x y) (sigmoid (aref z x y)))))
			(list tmp z)))
		((listp z) 	(list (mapcar #'sigmoid z) z))
		(t 			(/ 1 (+ 1 (expt e (* -1 z)))))))
  
(defun relu (x)
	"Computes the activation for matrix, scalar, or list x with a rectified linear unit"
	(cond 
		((is-matrix x)
			(let ((tmp (make-matrix (rowcount x) (columncount x) 0.0)))
				(loop for r from 0 to (- (rowcount x) 1)
					do (loop for y from 0 to (- (columncount x) 1)
						do (setf (aref tmp r y) (relu (aref x r y)))))
			(list tmp x)))
		((listp x) 	(list(mapcar #'relu x) x))
		(t 			(max 0 x))))

(defun initialize-parameters (dimensions parameters index)
	(unless (or (equal 1 (length dimensions)) (null dimensions))
		(setf (gethash (list 'w index) parameters) (make-matrix (second dimensions) (first dimensions))) ; weights
		(setf (gethash (list 'b index) parameters) (make-matrix (second dimensions) 1 0.0)) ; biases
		
		(initialize-parameters (rest dimensions) parameters (+ index 1))))

(defun forward-propagation (X parameters index caches)
	(let (
		(A X)
		(L (hash-table-count parameters))
		(g (layer-function index))
		(a-vals nil)
		(W (gethash (list 'w index) parameters))
		(b (gethash (list 'b index) parameters)))
			(print (list 'forward-propagation index))
			;(format t "~%caches: ~S~%" caches)
			(format t "~%layer function: ~S~%" g)
			;(format t "~%W: ~S~%" W)
			;(format t "~%b: ~S~%" b)
			
			(setq a-vals (linear-activation-forward A W b parameters g))
			
			(unless (equal index (- L 2))
				(forward-propagation (get-activation a-vals) parameters (+ index 1) caches))
			(setq caches (append caches (get-cache a-vals)))
			(format t "~%a-vals activation: ~S~%" (get-activation a-vals))
			
	(list (get-activation a-vals) caches)))

(defun linear-activation-forward (A_prev W b parameters activation-function)
	(let (
		(forward-vals nil)
		(a-vals nil))
	(setq forward-vals (linear-forward A_prev W b))
	
	; TODO is-sigmoid --> (setq a-vals (GET_ACTIVATION_F_HERE (get-Z forward-vals)))
	(cond 	((is-sigmoid activation-function) (setq a-vals (sigmoid (get-Z forward-vals))))
			((is-relu activation-function) (setq a-vals (relu (get-Z forward-vals))))
			(t (print '(activation function not supported))))
	
	(format t "~%activation is: ~S~%" (get-activation a-vals))
	;(format t "~%cache is ~S~%" (get-cache a-vals))
	
	; return the output of the activation function 
	; together with caches for backward propagation
	(list (get-activation a-vals) (list (get-linear-cache forward-vals) (get-cache a-vals)))))

(defun linear-forward (A_prev W b)
	(let ((Z nil))
		(format t "~%A_prev ~S~%" A_prev)
		(format t "~%W ~S~%" W)
		(format t "~%b ~S~%" b)
		;(setq Z (matadd (dot W A_prev) b))
		(setq Z (matadd (matmul W A_prev) b))
		(format t "~%Z ~S~%" Z)
	(list Z '(1 2 3))))

(defun is-sigmoid (activation-function)
	(string-equal activation-function 'SIGMOID))
	
(defun is-relu (activation-function)
	(string-equal activation-function 'RELU))
	
(defun get-Z (forward-values)
	(first forward-values))
	
(defun get-linear-cache (forward-values)
	(second forward-values))
	
(defun get-activation (activation-values)
	(first activation-values))
	
(defun get-cache (activation-values)
	(second activation-values))

(defun model (X Y learning_rate num_iterations)
	(let ((*parameters2* (make-hash-table :test 'equal))) ; weights and biases

		; initialize parameters
		(initialize-parameters (dimensions) *parameters2* 1)
		
		; loop for iterations
		(loop for l from 0 to (- num_iterations 1)
			do 
				(let ((forward-values nil))
					(setq forward-values (forward-propagation X *parameters2* 1 nil))
					(format t "~%A_L is ~S~%" (get-activation forward-values))
					(format t "cache is ~S~%" (get-cache forward-values))))
	*parameters2*))