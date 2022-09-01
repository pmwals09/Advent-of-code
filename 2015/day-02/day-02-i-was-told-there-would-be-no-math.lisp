(defun split-string (string &optional (delim-char #\Space))
  "Returns a list of substrings of 'string' divided by the delim-char, which
  defaults to a space character. NB: Two consecutive delimiters will be seen
  as if there's an empty string between. Taken from the Lisp Cookbook"
  (loop for i = 0 then (1+ j)
        as j = (position delim-char string :start i)
        collect (subseq string i j)
        while j))

(defun parse-dimensions (dimensions)
  "Parse dimensions into sides"
  (let ((num-dimensions (mapcar #'parse-integer dimensions)))
    (list (* (first num-dimensions) (second num-dimensions))
          (* (second num-dimensions) (third num-dimensions))
          (* (third num-dimensions) (first num-dimensions)))))

(defun parse-to-sides (box-string)
  "Parse the string from the input to the box sides sizes"
  (let* ((dimensions (split-string box-string #\x)))
    (parse-dimensions dimensions)))

(defun calculate-wrapping-paper (input-line)
  "Tally the amount of wrapping paper needed for a box in part one"
  (let* ((sides (parse-to-sides input-line))
         (min-side (reduce #'(lambda (out ea) (if (< ea out) ea out)) sides)))
    (+ min-side (reduce #'+ (mapcar (lambda (x) (* 2 x)) sides)))))

(defun calculate-wrapping-ribbon (input-line)
  "Calculate how much ribbon we need for a box in part two"
  (let* ((dimensions (mapcar #'parse-integer (split-string input-line #\x)))
         (max-dimension (reduce #'(lambda (out ea) (if (> ea out) ea out)) dimensions))
         (dimensions-no-max (remove-if (lambda (x) (= x max-dimension)) dimensions)))
    ; it's possible to have one low value with two equal high values. If that's
    ; the case, then add one back
    ; it's also possible to have a perfect cube! If that's the case, the list
    ; should be 2x the max-dimension
    (cond ((= 1 (length dimensions-no-max))
            (push max-dimension dimensions-no-max))
          ((= 0 (length dimensions-no-max))
           (setf dimensions-no-max (list max-dimension max-dimension))))
    ; add the smallest perimeter of any face and the volume
    (+ 
      (reduce '+ (mapcar (lambda (x) (* 2 x)) dimensions-no-max))
      (reduce '* dimensions))))

(with-open-file (f "./day-02-data.txt" :direction :input)
  (let ((total-paper 0)
        (total-ribbon 0))
    (do ((l (read-line f) (read-line f nil 'eof)))
        ((eq l'eof) nil)
      (setf total-paper (+ total-paper (calculate-wrapping-paper l)))
      (setf total-ribbon (+ total-ribbon (calculate-wrapping-ribbon l))))
    (format t "Part one: ~A~%" total-paper)
    (format t "Part two: ~A~%" total-ribbon)))
