(defun make-cd (title artist rating ripped)
  "Makes a CD list."
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-record (cd)
  "Adds a record to *db*."
  (push cd *db*))

(defun dump-db ()
  "Dumps the contents of *db* in a human friendly form."
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read (prompt)
  "Prompts the user to input a value for a field named `prompt`."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun parse-int-default-0 (var)
  "Parses `var` as integer. If value is unparsable, returns 0."
  ; parse-integer on junk returns nil, `or` will make it return 0
  (or (parse-integer var :junk-allowed t) 0))

(defun cd-from-prompt ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (parse-int-default-0 (prompt-read "Rating"))
   (prompt-read "Ripped [y/n]")))

(defvar *db* nil)
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))
(dump-db)
