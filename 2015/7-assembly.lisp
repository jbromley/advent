(defpackage :7-assembly
  (:use :cl :uiop)
  (:import-from :trivia #:match)
  (:import-from :alexandria #:hash-table-keys)
  (:export #:read-circuit))

(in-package :7-assembly)

(defun substringp (needle haystack &key (test 'char=))
  "Returns the index of the first occurrence of the string designated
by NEEDLE within the string designated by HAYSTACK, or NIL if it does
not occur.  Characters within the string are compared by TEST, which
defaults to CHAR= (for case-sensitive comparison)."
  (search (string needle) (string haystack) :test test))

(defun hash-set (h key value)
  (setf (gethash key h) value))

(defun hash-update (h key updater-fn default)
  (multiple-value-bind (value in-hash) (gethash key h)
    (if in-hash
	(setf (gethash key h) (funcall updater-fn value))
	(setf (gethash key h) (funcall updater-fn default)))))

(defun try-parse-integer (s)
  (let ((n (parse-integer s :junk-allowed t)))
    (if n n s)))

(defun and-16 (in1 in2)
  (logand in1 in2))

(defun or-16 (in1 in2)
  (logior in1 in2))

(defun lshift (in bits)
  (logand #xFFFF (ash in bits)))

(defun rshift (in bits)
  (ash in (- bits))) 

(defun parse-line (line graph gates)
  (cond
    ((or (member "AND" line :test #'string=)
	 (member "OR" line :test #'string=))
     (match line ((list x1 op x2 _ rhs)
       (let ((x1-num (try-parse-integer x1)))
         (hash-update graph x1 (lambda (v) (cons rhs v)) '())
         (hash-update graph x2 (lambda (v) (cons rhs v)) '())
	 (when (numberp x1-num)
	   (hash-set gates x1 x1-num))
         (hash-set gates rhs (list op x1 x2))))))
    ((or (member "LSHIFT" line :test #'string=)
	 (member "RSHIFT" line :test #'string=))
     (match line ((list x op num _ rhs)
		  (hash-update graph x (lambda (v) (cons rhs v)) '())
		  (hash-set gates rhs (list op x num)))))
    ((member "NOT" line :test #'string=)
     (match line ((list op x _ rhs)
		  (hash-update graph x (lambda (v) (cons rhs v)) '())
		  (hash-set gates rhs (list op x)))))
    ((not (parse-integer (first line) :junk-allowed t))
     (match line ((list x _ rhs)
		  (hash-update graph x (lambda (v) (cons rhs v)) '())
		  (hash-set gates rhs x))))
    (t
     (match line ((list num _ rhs)
		  ;; (hash-update graph num (lambda (v) (cons rhs v)) '())
		  (hash-set gates rhs (parse-integer num))))))
  (values graph gates))

(defun read-circuit (file)
  (let ((lines (uiop:read-file-lines file))
	(graph (make-hash-table :test 'equal))
	(gates (make-hash-table :test 'equal)))
    (loop for line in lines
	  do (parse-line (uiop:split-string line) graph gates))
    (values graph gates)))
