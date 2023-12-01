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

(defun join-str-list (str-lst)
  (format nil "~{~A~^~}" str-lst))

(defun parse-line (l)
  (labels parse-string (line &optional parts)
    (if (or (eq 0 (length line)) (null (search "\\" line)))
        (values (list (length l) (length l)))
        ()))
  (parse-string l))

(search "\\" "lltj\\\"kbbxi")
; parse-line("lltj\"kbbxi")
;   parse-string("lltj\"kbbxi" '())
;     parse-string("kbbxi" '("lltj))
;       parse-string("" '("lltj "kbbxi")

; parse-line("xziq\\\x18ybyv\x9am\"neacoqjzytertisysza")
; parse-string("xziq\\\x18ybyv\x9am\"neacoqjzytertisysza" '())
;   parse-string(x18ybyv\x9am\"neacoqjzytertisysza" '("xziq\))
;     parse-string(x9am\"neacoqjzytertisysza" '("xziq\ 8ybyv))
;       parse-string("neacoqjzytertisysza" '("xziq\ 8ybyv am))
;         parse-string("" '("xziq\ 8ybyv am "neacoqjzytertisysza"))
; 


; (with-open-file (f "./test.txt")
(with-open-file (f "./day-08-data.txt")
  (let ((total-code-char 0)
        (total-parse-char 0))
    (do ((l (read-line f nil) (read-line f nil)))
        ((null l))
      (destructuring-bind (word-code-char word-parse-char) (parse-line l)
        (format t "line: ~a length l: ~a, parsed-length l: ~a~%" l word-code-char word-parse-char)
        (setf total-code-char (+ total-code-char word-code-char))
        (setf total-parse-char (+ total-parse-char word-parse-char))))
    (format t "Part one: ~A~%" (- total-code-char total-parse-char))))
