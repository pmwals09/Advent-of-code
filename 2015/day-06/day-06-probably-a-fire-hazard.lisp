(defstruct point row col)

(defun make-grid (n &key (initial-element 0) )
  (make-array (list n n) :initial-element initial-element))

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

(defun apply-to-range (start-point end-point on-off fn grid)
  (do ((row (point-row start-point) (+ 1 row)))
      ((> row (point-row end-point)) nil)
    (do ((col (point-col start-point) (+ 1 col)))
        ((> col (point-col end-point)) nil)
      (setf (aref grid row col) (funcall fn (aref grid row col) on-off)))))

(defun make-point-from-list (point-list)
  (make-point 
    :row (parse-integer (car point-list)) 
    :col (parse-integer (cadr point-list))))

(defun make-point-from-string (str)
  (make-point-from-list (split-string #\, str)))

(defun parse-line (line)
  (let ((tokens (split-string #\Space line)))
    (if (equal (car tokens) "toggle")
        (values 
          (make-point-from-string (cadr tokens))
          (make-point-from-string (cadddr tokens)))
        (values
          (make-point-from-string (caddr tokens))
          (make-point-from-string (cadr (cdddr tokens)))
          (cadr tokens)))))

(defun tally-grid (grid)
  (reduce #'+ (make-array (array-total-size grid) :displaced-to grid)))

(defun handle-lights (x on-off)
  (if (null on-off)
      (if (= x 1) 0 1)
      (if (equal on-off "on") 1 0)))

(defun handle-brightness (x on-off)
  (if (null on-off)
      (+ x 2)
      (if (equal on-off "on") 
          (+ x 1) 
          (apply #'max (list 0 (- x 1))))))

(with-open-file (f "./day-06-data.txt")
  (let ((part-one-grid (make-grid 1000))
        (part-two-grid (make-grid 1000)))
    (do ((l (read-line f nil) (read-line f nil)))
        ((null l))
            (multiple-value-bind (start-point end-point on-off) (parse-line l)
              (apply-to-range 
                start-point 
                end-point 
                on-off
                #'handle-lights
                part-one-grid)
              (apply-to-range
                start-point
                end-point
                on-off
                #'handle-brightness
                part-two-grid)))
    (format t "Part one: ~a~%" (tally-grid part-one-grid))
    (format t "Part two: ~a~%" (tally-grid part-two-grid))))
