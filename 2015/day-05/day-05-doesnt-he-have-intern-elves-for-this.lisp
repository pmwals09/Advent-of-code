(defun str-to-c-list (str)
  "Convert a string to a list of characters"
  (map 'list #'(lambda (x) x) str))

(defun is-not-vowel-p (c)
  "Predicate: is a character a vowel?"
  (every (lambda (vowel) (not (eq c vowel))) (str-to-c-list "aeiou")))

(defun has-three-vowels-p (word)
  "Does a string have at least 3 vowels"
  (<= 3 (length (remove-if #'is-not-vowel-p (str-to-c-list word)))))

(defun double-letter-p (word)
  "Is there at least one double letter in a string"
  (cond ((= 1 (length word)) nil)
        ((eq (char word 0) (char word 1)) t)
        (t (double-letter-p (subseq word 1)))))

(defun no-forbidden-pairs-p (word)
  "Does this list of 'forbidden' substrings appear in a superstring"
  (let ((forbidden-pairs '("ab" "cd" "pq" "xy")))
    (not (some (lambda (pair) (not (eq nil (search pair word)))) forbidden-pairs))))

(defun is-good-word-p1-p (word)
  (and 
    (no-forbidden-pairs-p word)
    (has-three-vowels-p word)
    (double-letter-p word)))

(defun leading-pair-in-reminder-p (str)
  "Does the leading pair of a string appear in the remainder of the string"
  (search (subseq str 0 2) (subseq str 2)))

(defun two-pairs-p (word)
  "Is there some character pair in a word that repeats"
  (dotimes (i (- (length word) 1) nil)
    (when (not (null (leading-pair-in-reminder-p (subseq word i))))
      (return t))))

(defun pair-one-between-p (word)
  "Is there a 'split pair' of characters with one separating character"
  (cond ((= 2 (length word)) nil)
        ((eq (char word 0) (char word 2)) t)
        (t (pair-one-between-p (subseq word 1)))))

(defun is-good-word-p2-p (word)
  (and
    (two-pairs-p word)
    (pair-one-between-p word)))

(with-open-file (f "./day-05-data.txt" :direction :input)
  (let ((good-words-p1 0)
        (good-words-p2 0))
    (do ((l (read-line f) (read-line f nil 'eof)))
      ((eq l 'eof) nil)
      (when (is-good-word-p1-p l) (setf good-words-p1 (+ 1 good-words-p1)))
      (when (is-good-word-p2-p l) (setf good-words-p2 (+ 1 good-words-p2))))
    (format t "Part one: ~a~%" good-words-p1)
    (format t "Part two: ~a~%" good-words-p2)))
