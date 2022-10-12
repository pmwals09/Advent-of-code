(defparameter *grid* (make-array (list 1000 1000) :initial-element 0))
(defstruct point row col)

(defun split-string (delimiter str)
  (cond ((null (position delimiter str))
         (cons str nil))
        ((= 0 (position delimiter str))
         (split-string delimiter (subseq str 1)))
        ((= (- (length str) 1) (position delimiter str))
         (subseq str 0 (- (length str) 1)))
        (t
         (cons 
           (subseq str 0 (position delimiter str)) 
           (split-string delimiter (subseq str (position delimiter str)))))))

(defun apply-to-range (start-point end-point fn)
  (do ((row (point-row start-point) (+ 1 row)))
      ((> row (point-row end-point)) nil)
    (do ((col (point-col start-point) (+ 1 col)))
        ((> col (point-col end-point)) nil)
      (setf (aref *grid* row col) (funcall fn (aref *grid* row col))))))

(defun make-point-from-list (point-list)
  (make-point :row (parse-integer (car point-list)) :col (parse-integer (cadr point-list))))

(defun make-point-from-string (str)
  (make-point-from-list (split-string #\, str)))

(defun parse-line (tokens)
    (if (equal (car tokens) "toggle")
        (values 
          (make-point-from-string (cadr tokens))
          (make-point-from-string (cadddr tokens)))
        (values
          (cadr tokens)
          (make-point-from-string (caddr tokens))
          (make-point-from-string (cadr (cdddr tokens))))))

(with-open-file (f "./day-06-data.txt")
  (do ((l (read-line f nil) (read-line f nil)))
      ((null l))
    (let ((tokens (split-string #\Space l)))
      (if (equal "toggle" (car tokens))
          (multiple-value-bind (start-point end-point) (parse-line tokens)
            (apply-to-range start-point end-point #'(lambda (x) (if (= x 1) 0 1))))
          (multiple-value-bind (on-off start-point end-point) (parse-line tokens)
            (apply-to-range start-point end-point #'(lambda (x) (if (equal on-off "on") 1 0)))))))
  (reduce #'+ (make-array (array-total-size *grid*) :displaced-to *grid*)))
