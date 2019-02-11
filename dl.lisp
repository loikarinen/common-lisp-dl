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

;
; FORWARD PROPAGATION
;

(defun forward-propagation (X parameters index caches accumulator)
	(let (
		(A X)
		(L (model-layer-count))
		(g (layer-function index))
		(a-vals nil)
		(W (gethash (list 'w index) parameters))
		(b (gethash (list 'b index) parameters)))
			(print (list 'forward-propagation index))
			;(format t "~%caches: ~S~%" caches)
			(format t "~%layer function: ~S~%" g)
			;(format t "~%L: ~S~%" L)
			;(format t "~%b: ~S~%" b)
			(setq a-vals (linear-activation-forward A W b parameters g))
			(setq caches (append caches (get-cache a-vals)))
			
			(cond ((equal index (- L 1)) 
						(format t "~%*******************************************~%" b)
						(format t "~%a-vals activation: ~S~%" (get-activation a-vals))
						(setq accumulator (list (get-activation a-vals) caches)))
				   (t (forward-propagation (get-activation a-vals) parameters (+ index 1) caches accumulator)))))

(defun linear-activation-forward (A_prev W b parameters activation-function)
	(let ((forward-vals nil)
		  (a-vals nil))
		(setq forward-vals (linear-forward A_prev W b))
		
		; TODO is-sigmoid --> (setq a-vals (GET_ACTIVATION_F_HERE (get-Z forward-vals)))
		(cond 	((is-sigmoid activation-function) (setq a-vals (sigmoid (get-Z forward-vals))))
				((is-relu activation-function) (setq a-vals (relu (get-Z forward-vals))))
				(t (print '(activation function not supported))))
		
		;(format t "~%activation is: ~S~%" (get-activation a-vals))
		;(format t "~%cache is ~S~%" (get-cache a-vals))
		
		; return the output of the activation function 
		; together with caches for backward propagation
	(list (get-activation a-vals) (list (get-linear-cache forward-vals) (get-cache a-vals)))))

(defun linear-forward (A_prev W b)
	(let ((Z nil))
		;(format t "~%A_prev ~S~%" A_prev)
		;(format t "~%W ~S~%" W)
		;(format t "~%b ~S~%" b)
		(setq Z (mat-add (matmul W A_prev) b))
		;(format t "~%Z ~S~%" Z)
	(list Z '(1 2 3))))

;
; COST
;
	
(defun compute-cost (A_L Y)
	(let ((f (model-cost-function)))
		(format t "~%cost-function ~S~%" f)
		(cond 	((is-cross-entropy f) (cross-entropy A_L Y))
				(t (print '(cost function not supported))))))

(defun cross-entropy (A_L Y)
	(let ((m (array-dimension Y 0))) ; number of labels
		(* (/ -1 m) (add-column (mat-add 
			(element-wise-multiply Y (matrix-map (lambda (x) (log x)) A_L))
			(element-wise-multiply (matrix-map (lambda (x) (- 1 x)) Y) (matrix-map (lambda (x) (log (- 1 x))) A_L)))))))
			
(defun is-sigmoid (activation-function)
	(string-equal activation-function 'SIGMOID))
	
(defun is-relu (activation-function)
	(string-equal activation-function 'RELU))
	
(defun is-cross-entropy (cost-function)
	(string-equal cost-function 'CROSS-ENTROPY))
	
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
		(format t "~%Dimensions are ~S~%" (model-dimensions))
		; initialize parameters
		(initialize-parameters (model-dimensions) *parameters2* 1)
		(loop for key being the hash-keys of *parameters2*
           using (hash-value value)
           do (format t "The value associated with the key ~S is ~S~%" key value))
		; loop for iterations
		(loop for l from 0 to (- num_iterations 1)
			do 
				(let ((forward-values nil)
					  (cost nil)
					  (gradients nil)
					  (new_parameters nil))
					(setq forward-values (forward-propagation X *parameters2* 1 nil nil))
					(format t "~%A_L is ~S~%" (get-activation forward-values))
					(format t "cache is ~S~%" (get-cache forward-values))
					(setq cost (compute-cost (get-activation forward-values) Y))
					(format t "~%cost: ~S~%" cost)
					
					;(setq gradients (backward-propagation (get-activation forward-values) Y (get-cache forward-values)))
					;(setq new_parameters (update-parameters *parameters2* gradients learning_rate))
					))
	*parameters2*))