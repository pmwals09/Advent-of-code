(defclass Point ()
  ((x
     :initarg :x
     :initform 0
     :accessor x)
   (y
     :initarg :y
     :initform 0
     :accessor y)))

(defun handle-direction (direction point delivery-hash)
  (cond ((eq direction #\^) (decf (slot-value point 'y)))
        ((eq direction #\>) (incf (slot-value point 'x)))
        ((eq direction #\v) (incf (slot-value point 'y)))
        ((eq direction #\<) (decf (slot-value point 'x))))
  (multiple-value-bind (val exists?) (gethash (slot-value point 'y) delivery-hash)
    (if (not (null exists?))
        (push (slot-value point 'x) (gethash (slot-value point 'y) delivery-hash))
        (setf (gethash (slot-value point 'y) delivery-hash) (list (slot-value point 'x))))))

(with-open-file (f "./day-03-data.txt" :direction :input)
; (with-open-file (f "./test.txt" :direction :input)
  (let ((santa-location (make-instance 'Point))
        (robo-santa-location (make-instance 'Point))
        (split-santa-location (make-instance 'Point))
        (delivery-hash (make-hash-table))
        (robo-delivery-hash (make-hash-table))
        (total-houses 0)
        (total-robo-houses 0))
    (setf (gethash 0 delivery-hash) '(0))
    (setf (gethash 0 robo-delivery-hash) '(0))
    (do ((c (read-char f) (read-char f nil))
         (i 0 (+ i 1)))
        ((not (characterp c)))
      (handle-direction c santa-location delivery-hash)
      (if (= 0 (mod i 2))
          (handle-direction c split-santa-location robo-delivery-hash)
          (handle-direction c robo-santa-location robo-delivery-hash)))
    (maphash #'(lambda (k v) (incf total-houses (length (remove-duplicates v)))) delivery-hash)
    (maphash #'(lambda (k v) (incf total-robo-houses (length (remove-duplicates v)))) robo-delivery-hash)
    (format t "Part one: ~A~%" total-houses)
    (format t "Part two: ~A~%" total-robo-houses)))
