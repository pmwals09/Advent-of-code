(defun split-string (delimiter str)
  (cond ((null (search delimiter str))
         (cons str nil))
        ((= 0 (search delimiter str))
         (split-string 
           delimiter 
           (subseq str (length delimiter))))
        ((= (- (length str) 1) (search delimiter str))
         (subseq str 0 (- (length str) 1)))
        (t
         (cons
           (subseq str 0 (search delimiter str))
           (split-string 
             delimiter 
             (subseq str (+ (search delimiter str) (length delimiter))))))))

(defun bw-and (a b)
  (logand a b))

(defun bw-or (a b)
  (logior a b))

(defun bw-lshift (val amt)
  (ash val (values (parse-integer amt))))

(defun bw-rshift (val amt)
  (ash val (* -1 (values (parse-integer amt)))))

(defun bw-not (a)
  (values 
    (values (parse-integer 
      (map 
        'string 
        #'(lambda (c) (if (equal c #\0) #\1 #\0))
        (format nil "~16,'0b" a)) 
      :radix 2))))

(defun get-target-wire (target-wire instructions results)
  (handler-case
      (let ((res (values (parse-integer target-wire))))
        (setf (gethash target-wire results)(values (parse-integer target-wire)))
        res)
    (error ()
      (when (not (nth-value 1 (gethash target-wire results)))
        (let* ((gate (gethash target-wire instructions))
               (tokens (split-string " " gate)))
          (setf (gethash target-wire results) 
                (cond ((<= (length gate) 2) 
                       (get-target-wire gate instructions results))
                      ((not (null (search "AND" gate)))
                       (bw-and 
                         (get-target-wire (car tokens) instructions results) 
                         (get-target-wire (caddr tokens) instructions results)))
                      ((not (null (search "OR" gate)))
                       (bw-or
                         (get-target-wire (car tokens) instructions results)
                         (get-target-wire (caddr tokens) instructions results)))
                      ((not (null (search "LSHIFT" gate)))
                       (bw-lshift
                         (get-target-wire (car tokens) instructions results)
                         (caddr tokens)))
                      ((not (null (search "RSHIFT" gate)))
                       (bw-rshift
                         (get-target-wire (car tokens) instructions results)
                         (caddr tokens)))
                      ((not (null (search "NOT" gate)))
                       (bw-not (get-target-wire (cadr tokens) instructions results)))
                      (t
                       (parse-integer gate))))))
      (gethash target-wire results))))

(with-open-file (f "./day-07-data.txt")
  (let ((instructions (make-hash-table :test 'equal))
        (results (make-hash-table :test 'equal)))
    (do ((l (read-line f nil) (read-line f nil)))
        ((null l))
      (destructuring-bind (gate wire) (split-string " -> " l)
        (setf (gethash wire instructions) gate)))
    (let ((wire-a (format nil "~a" (get-target-wire "a" instructions results))))
      (format t "Part one: ~a~%" (get-target-wire "a" instructions results))
      (setf (gethash "b" instructions) wire-a)
      (setf results (make-hash-table :test 'equal))
      (format t "Part two: ~a~%" (get-target-wire "a" instructions results)))))
