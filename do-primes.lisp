;;; doprimes.lisp
;;;
;;; Iteration over successive prime numbers using macros.

(defun primep (number)
  "Returns true if `number` is a prime number."
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  "If `number` is a prime number, returns it. If not, returns the next prime number."
  (loop for n from number when (primep n) return n))

;; Expected syntax for the macro:
;; (do-primes (p 0 19)
;;   (format t "~d " p))
;;
;; Same logic without a macro:
;; (do ((p (next-prime 0) (next-prime (1+ p))))
;;     ((> p 19))
;;   (format t "~d " p))

(defmacro do-primes ((var start end) &body body)
  "Iterates over all prime numbers in the specified range."
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
