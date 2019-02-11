(defparameter *model*
	'((0 => 3 nil)
	  (1 => 5 "relu")
	  (2 => 1 "sigmoid")
	  (-1 "cost-function" => "cross-entropy")
	  (-2 "optimization" => "adam"))
	"The deep learning neural net architecture. 
	Format is (LAYER_NR: LAYER_NODES ACTIVATION_FUNCTION).")

(defun nodes-on-layer (index)
	"The node count on layer index."
	(nth 2 (assoc index *model*)))

(defun layer-function (index)
	"The layer activation function."
	(nth 3 (assoc index *model*)))

(defun model-cost-function ()
	"The model's cost function." 
	(nth 3 (assoc -1 *model*)))

(defun model-optimization-algorithm ()
	"The model's cost function." 
	(nth 3 (assoc -2 *model*)))
	
(defun model-layer-count ()
	"The model's optimization algorithm." 
	(length (model-dimensions)))
	
(defun model-dimensions ()
	"Ordered list of node counts per layer."
	(remove nil (mapcar (lambda (x)  ; remove NIL values from returned list
		(let ((index (first x)))
			(unless (< index 0) ; NIL for negative indices
				(nodes-on-layer (first x))))) *model*)))
				