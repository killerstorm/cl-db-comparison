(require 'cl-prevalence)

(defvar *system* ())

(defclass persistent-array ()
  ((id :accessor id-of :initarg :id)
   (array :accessor array-of :initarg :array)))

(defun tx-make-persistent-array (system id dims)
  (setf (cl-prevalence:get-root-object system id)
	(make-instance 'persistent-array :id id
		       :array (make-array dims :adjustable t))))

(defun make-persistent-array (id dims)
  (cl-prevalence:execute-transaction (tx-make-persistent-array *system* id dims)))

(defun tx-adjust-array-dimensions (system pa-id new-dimensions)
  (adjust-array (array-of (cl-prevalence:get-root-object system pa-id))
		new-dimensions))

(defmethod adjust-array-dimensions ((pa persistent-array) new-dimensions)
  (cl-prevalence:execute-transaction (tx-adjust-array-dimensions *system* (id-of pa) new-dimensions))
  pa)

(defmethod paref ((pa persistent-array) &rest subscripts)
  (apply 'aref (array-of pa) subscripts))

(defun tx-setf-paref (system pa-id new-value subscripts)
  (apply '(setf aref) new-value (array-of (cl-prevalence:get-root-object system pa-id)) subscripts))

(defmethod (setf paref) (new-value (pa persistent-array) &rest subscripts)
  (cl-prevalence:execute-transaction (tx-setf-paref *system* (id-of pa) new-value subscripts)))


(defun open-store (pathname &key auto-set-globals)
  (when *system*
    (close-store))
  (setf *system* (cl-prevalence:make-prevalence-system pathname))
  (when auto-set-globals 
    (maphash (lambda (name array)
	       (setf (symbol-value name) array))
	     (cl-prevalence::get-root-objects *system*))))

(defun close-store ()
  (cl-prevalence::close-open-streams *system*)
  (setf *system* nil))
  