(defparameter *model*
	'((0 => 3 nil)
	  (1 => 5 "sigmoid")
	  (2 => 2 "relu"))
	"The deep learning neural net architecture. 
	Format is (LAYER_NR: LAYER_NODES ACTIVATION_FUNCTION).")

(defun layer-count (index)
	"The layer node count."
	(first (rest (rest (assoc index *model*)))))

(defun layer-function (index)
	"The layer activation function."
	(second (rest (rest (assoc index *model*)))))

(defun dimensions ()
	(mapcar (lambda (x) (layer-count (first x))) *model*))