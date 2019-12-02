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
		(t 			(min 0.999999915765362456 (/ 1 (+ 1 (expt e (* -1 z))))))))
  
(defun sigmoid-backward (dA layer-cache)
	"Computes the derivative of the sigmoid from cached values"
	; Sigmoid derivative = s(x)(1 − s(x))
	(let ((s (get-activation-cache layer-cache)))
		(element-wise-multiply dA (element-wise-multiply s (matrix-map (lambda (x) (- 1 x)) s)))))
  
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

(defun relu-backward (dA layer-cache)
	"Computes the derivative of the relu from cached values"
	; ReLU derivative = 0 if x < 0 else 1
	(let ((r (get-activation-cache layer-cache)))
	(element-wise-multiply dA (matrix-map (lambda (x) (cond ((< x 0) 0) (t 1))) r))))

(defun initialize-parameters (dimensions parameters index)
	(unless (or (equal 1 (length dimensions)) (null dimensions))
		(setf (gethash (list 'w index) parameters) (mat-multiply (make-matrix (second dimensions) (first dimensions)) 0.01)) ; weights
		(setf (gethash (list 'b index) parameters) (make-matrix (second dimensions) 1 0.0)) ; biases
		
		(initialize-parameters (rest dimensions) parameters (+ index 1))))

;
; FORWARD PROPAGATION
;
; Cache list format repeated L times: 
; (( A W b) activation)
;
(defun forward-propagation (X parameters index caches accumulator)
	(let (
		(A X)
		(L (model-layer-count))
		(g (layer-function index))
		(a-vals nil)
		(W (gethash (list 'w index) parameters))
		(b (gethash (list 'b index) parameters)))
			(setq a-vals (linear-activation-forward A W b parameters g))
			(setq caches (append caches (second a-vals)))
			
			(cond ((equal index (- L 1)) 
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
				
		; return the output of the activation function 
		                        ; together with cache for backward propagation
	(list (get-activation a-vals) (list(list (get-linear-cache forward-vals) (get-activation-cache a-vals))))))

(defun linear-forward (A_prev W b)
	(let ((Z nil))
		(setq Z (mat-add (transpose (matmul (transpose A_prev) (transpose W))) b))
	(list Z (list A_prev W b))))

;
; COST
;
	
(defun compute-cost (A_L Y)
	(let ((f (model-cost-function)))
		(cond 	((is-cross-entropy f) (cross-entropy A_L Y))
				(t (print '(cost function not supported))))))

(defun cross-entropy (A_L Y)
	(let (
		(m (array-dimension Y 0))); number of labels
			(* (/ -1 m) (add-column (mat-add 
				(element-wise-multiply Y (matrix-map (lambda (x) (log x)) A_L))
				(element-wise-multiply (matrix-map (lambda (x) (- 1 x)) Y) (matrix-map (lambda (x) (log (- 1 x))) A_L)))))))
			
;
; BACKWARD PROPAGATION
;

(defun backward-propagation (A_L Y caches)
	(let (
		(L (length caches))
		(Y_T (transpose Y))
		(index (length caches))
		(gradients nil))
			(activation-backward 
				; dA_L
				(mat-multiply (element-wise-minus (element-wise-divide Y_T A_L) (element-wise-divide (matrix-map (lambda (x) (- 1 x)) Y_T) (matrix-map (lambda (x) (- 1 x)) A_L))) -1) 
				caches 
				index 
				nil)))

;
; Gradients list format repeated L times: 
; (dA dW db) 
;
; index matches layer number (ie. L is last layer) for ease of reading

(defun activation-backward (dA caches index accumulator)
		(let (
			(cache nil)
			(linear_cache nil)
			(m nil)
			(dZ nil)
			(dW nil)
			(db nil)
			(dA_prev nil)
			(layer_gradients))
		(setq cache 		(nth (- index 1) caches))
		(setq linear_cache 	(first cache))
		(setq m 			(columncount (first linear_cache)))
		(setq dZ 
			(cond ((is-sigmoid 	(layer-function index)) (sigmoid-backward dA cache))
				  ((is-relu 	(layer-function index)) (relu-backward dA cache))
				  (t 			(print '(activation derivative function not supported)))))
		(setq dW 		(mat-multiply (matmul dZ (transpose (first linear_cache))) (/ 1 m)))
		(setq db  		(sum-axis dZ 1))
		(setq dA_prev 	(matmul (transpose (second linear_cache)) dZ))
		
		(setq layer_gradients (list dA_prev dW db))
		(cond ((null accumulator) 	(setq accumulator layer_gradients))
			  (t 				 	(setq accumulator (list nil layer_gradients accumulator))))
		
		; sanity check for gradient shapes
		(check-gradient-shapes dA_prev dW db linear_cache)
			
		(cond ((> index 1) (activation-backward dA_prev caches (- index 1) accumulator))
			  ; return
			  ((= index 1) accumulator))))

(defun check-gradient-shapes (dA_prev dW db linear_cache)
	(assert (eql (columncount dA_prev) (columncount (first linear_cache)))
		(dA_prev (first linear_cache))
		"dA_prev and A_prev column counts don't match")
	(assert (eql (rowcount dA_prev) (rowcount (first linear_cache)))
		(dA_prev (first linear_cache))
		"dA_prev and A_prev row counts don't match")
	(assert (eql (columncount dW) (columncount (second linear_cache)))
		(dW (second linear_cache))
		"dW and W column counts don't match")
	(assert (eql (rowcount dW) (rowcount (second linear_cache)))
		(dW (second linear_cache))
		"dW and W row counts don't match")
	(assert (eql (columncount db) (columncount (third linear_cache)))
		(db (third linear_cache))
		"db and b column counts don't match")
	(assert (eql (rowcount db) (rowcount (third linear_cache)))
		(db (third linear_cache))
		"db and b row counts don't match"))

(defun update-parameters (parameters new_parameters gradients learning-rate)
	(loop for l from 1 to (- (model-layer-count) 1)
			do 
				(setf (gethash (list 'w l) new_parameters) (element-wise-minus (gethash (list 'w l) parameters) (mat-multiply (nth l (second gradients)) learning-rate))) ; weights
				(setf (gethash (list 'b l) new_parameters) (element-wise-minus (gethash (list 'b l) parameters) (mat-multiply (nth l (third gradients)) learning-rate))) ; biases
				)
	new_parameters)
		
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
	
(defun get-activation-cache (activation-values)
	(second activation-values))

(defun print-accuracy (activations Y index)
	(unless (>= index (array-total-size activations))
		(print-accuracy activations Y (+ index 1))))
	
(defun model (X Y learning_rate num_iterations)
	(let (
		(*parameters* (make-hash-table :test 'equal)) ; weights and biases
		(new_parameters (make-hash-table :test 'equal)))
		(format t "~%parameters count ~S~%" (hash-table-size *parameters*))
			(format t "~%Dimensions are ~S~%" (model-dimensions))
			
			(initialize-parameters (model-dimensions) *parameters* 1)
			
			; loop for iterations
			(loop for l from 0 to (- num_iterations 1)
				do 
					(let ((forward-values nil)
						  (cost nil)
						  (gradients nil))

						(setq forward-values (forward-propagation X *parameters* 1 nil nil))
						(setq cost (compute-cost (get-activation forward-values) Y))
						(setq gradients (backward-propagation (get-activation forward-values) Y (get-activation-cache forward-values)))
						(setq new_parameters (update-parameters *parameters* new_parameters gradients learning_rate))
						(setq *parameters* new_parameters)
					))
	new_parameters))