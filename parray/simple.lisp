(defvar *p-log* nil)

(defun log-op (op &rest args)
  (cond 
    ((not *p-log*) (error "store is not open"))
    ((eq *p-log* :inhibited)
     ())
    (t (let ((*print-readably* t)
	     (*print-pretty* nil))
	 (prin1 (list* op args) *p-log*)
	 (terpri *p-log*)))))

(defvar *persistent-arrays* (make-hash-table :test 'equal))

(defun find-parray (id)
  (gethash id *persistent-arrays*))

(defclass persistent-array ()
  ((id :accessor id-of :initarg :id)
   (array :accessor array-of :initarg :array)))

(defmethod initialize-instance :after ((pa persistent-array) &key)
  (setf (gethash (id-of pa) *persistent-arrays*) pa)
  (log-op :make-persistent-array (id-of pa) (array-dimensions (array-of pa))))

(defun make-persistent-array (id dims)
  (make-instance 'persistent-array :id id :array (make-array dims :adjustable t)))

(defmethod adjust-array-dimensions ((pa persistent-array) new-dimensions)
  (adjust-array (array-of pa) new-dimensions)
  (log-op :adjust-array-dimensions (id-of pa) new-dimensions)
  pa)

(defmethod paref ((pa persistent-array) &rest subscripts)
  (apply 'aref (array-of pa) subscripts))

(defmethod (setf paref) (new-value (pa persistent-array) &rest subscripts)
  (apply '(setf aref) new-value (array-of pa) subscripts)
  (log-op :setf-paref (id-of pa) new-value subscripts)
  new-value)

(defun open-store (pathname &key auto-set-globals)
  (when *p-log* (close-store))
  (with-open-file (s pathname :if-does-not-exist nil)
    (when s
      (let ((*p-log* :inhibited))
	(loop for cmd = (read s nil nil)
	      while cmd
	      do (destructuring-bind (op id . args)
		     cmd
		   (ecase op
		     (:make-persistent-array 
		      (make-persistent-array id (first args)))
		     (:adjust-array-dimensions 
		      (adjust-array-dimensions (find-parray id) (first args)))
		     (:setf-paref 
		      (let ((new-value (first args))
			    (subscripts (second args)))
			(apply '(setf paref) new-value (find-parray id) subscripts)))))))))
  (when auto-set-globals 
    (maphash (lambda (name array)
	       (setf (symbol-value name) array))
	     *persistent-arrays*))
  (setf *p-log* (open pathname :direction :output :if-exists :append :if-does-not-exist :create)))

(defun close-store ()
  (close *p-log*)
  (setf *p-log* nil)
  (setf *persistent-arrays* (make-hash-table :test 'equal))
  ())
