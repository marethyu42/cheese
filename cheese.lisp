(defun cheese (word)
  (print word)
  (cond
   ((vowelp (string-downcase (subseq word 0 1))) (concatenate 'string "ch" word))
   (t (cheese (subseq word 1 (length word))))))

(defun vowelp (letter)
  (inside letter (list "a" "e" "i" "o" "u" "y")))
             
(defun inside (item lst)
  (cond
   ((not lst) nil)
   ((equal (car lst) item) t)
   (t (inside item (cdr lst)))))

(defun chotal (input)
  (chotal-logic (mapcar #'cheese (my-split input))))

(defun chotal-logic (lst &optional (new-str ""))
  (if lst
      (chotal-logic (cdr lst) (concatenate 'string new-str (string #\Space) (car lst)))
      new-str))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
      :while end))

(defun delimiterp (c) (or (char= c #\Space)))


(chotal "Hello There")
