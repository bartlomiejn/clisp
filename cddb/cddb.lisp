;;; cddb.lisp
;;;
;;; Persistence and CRUD operations for a list of hypothetical CDs.

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
  ;; `parse-integer` on non-int value throws an error, :junk-allowed returns nil. `or` will make it return 0
  (or (parse-integer var :junk-allowed t) 0))

(defun cd-from-prompt ()
  "Creates a cd list "
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (parse-int-default-0 (prompt-read "Rating"))
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  "Prompts to add a list of CDs to *db*."
  (loop (add-record (cd-from-prompt))
        (if (not (y-or-n-p "Another record?: ")) (return))))

(defun save-db (filename)
  "Saves contents of *db* to filename"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  "Loads contents of *db* from filename"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

;;; CRUD Operations

(defmacro where (&rest clauses)
  "Constructs a selector function for use with `select`."
  ;; The difference is this implementation is a macro. 
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

(defun where-as-fun (&key title artist rating (ripped nil ripped-p))
  "Constructs a selector function for use with `select`, with provided properties matching the fields."
  #'(lambda (cd)
      (and
       ;; if syntax is more like ternary op in c-like languages: if (condition) action; else action;
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun select (selector-fn)
  "Returns records that match selector function."
  (remove-if-not selector-fn *db*))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  "Updates records that match the selector function with provided fields."
  (setf *db*
        (mapcar ; Maps over a list
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))
