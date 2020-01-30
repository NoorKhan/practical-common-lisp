(defun hello-world ()
  (format t "Hello, world!"))

(hello-world)

(defun load-db (filename)
  (with-open-file (in filename
		      :if-does-not-exist nil)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defvar *filename* "my-cds.db")
(load-db *filename*)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd) (push cd *db*))

(defun seed-db ()
  (progn (add-record (make-cd "Roses" "Kathy Mattea" 7 t))
	 (add-record (make-cd "Fly" "Dixie Chicks" 8 t))
	 (add-record (make-cd "Home" "Dixie Chicks" 9 t))))

(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-for-rating)
   (y-or-n-p "Ripped [y/n]: ")))

;; Need to select rating from [0, 10]
(defun prompt-for-rating ()
  (let ((rating (parse-integer (prompt-read "Rating") :junk-allowed t)))
    (cond ((equal rating nil) 0)
	  ((or (< rating 0) (> rating 10))
	   (progn (format t "Rating needs to between [0, 10]~%")
		  (prompt-for-rating)))
	  (t rating))))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equalp (getf cd :artist) artist))
   *db*))

(defun foo (&key a b c)
  (progn
    (format t (if a "has a~%" "no a~%"))
    (format t (if b "has b~%" "no b~%"))
    (format t (if c "has c~%" "no c~%"))))

(foo :a 1 :c 2)
(foo)
