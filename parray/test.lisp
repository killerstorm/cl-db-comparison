(defvar *array-store-name* #p"my.arrays")
(defvar *array-store-name* #p"my.arrays.p/")

(open-store *array-store-name*)

(defvar *myarray* (make-persistent-array '*myarray* '(2 2)))

(adjust-array-dimensions *myarray* '(3 3))

(dotimes (i 3)
  (dotimes (j 3)
    (setf (paref *myarray* i j) (+ (* 10 i) j))))

(dotimes (i 3)
	   (dotimes (j 3)
	     (princ (paref *myarray* i j))
	     (princ " "))
	   (terpri))

(close-store)

(setf *myarray* nil)

(open-store *array-store-name* :auto-set-globals t)

(dotimes (i 3)
	   (dotimes (j 3)
	     (princ (paref *myarray* i j))
	     (princ " "))
	   (terpri))

(close-store)