(defpackage :7-assembly
  (:use :cl :uiop :trivia))

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

(defun and-16 (in1 in2)
  (logand in1 in2))

(defun or-16 (in1 in2)
  (logior in1 in2))

(defun lshift (in bits)
  (logand #xFFFF (ash in bits)))

(defun rshift (in bits)
  (ash in (- bits))) 

(defun parse-line (line g calc)
  (cond
    ((or (member "AND" line :test #'string=)
	 (member "OR" line :test #'string=))
     (match line ((list x1 op x2 _ rhs)
       (hash-update g x1 (lambda (v) (cons rhs v)) '())
       (hash-update g x2 (lambda (v) (cons rhs v)) '())
       (hash-set calc rhs (list op x1 x2)))))
    ((or (member "LSHIFT" line :test #'string=)
	 (member "RSHIFT" line :test #'string=))
     (match line ((list x op num _ rhs)
		  (hash-update g x (lambda (v) (cons rhs v)) '())
		  (hash-set calc rhs (list op x num)))))
    ((member "NOT" line :test #'string=)
     (match line ((list op x _ rhs)
		  (hash-update g x (lambda (v) (cons rhs v)) '())
		  (hash-set calc rhs (list op x)))))
    ((not (parse-integer (first line) :junk-allowed t))
     (match line ((list x _ rhs)
		  (hash-update g x (lambda (v) (cons rhs v)) '())
		  (hash-set calc rhs (list "value" x)))))
    (t
     (match line ((list num _ rhs)
		  (hash-set calc rhs (parse-integer num))))))
  (values g calc))


(defun read-circuit (file)
  (let ((lines (uiop:read-file-lines file))
	(g (make-hash-table))
	(gates (make-hash-table)))
    (loop for line in lines
	  collect (uiop:split-string line))))
