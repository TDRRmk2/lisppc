(cl:defpackage "lisppc"
  (:use "uiop"))

(cl:in-package "lisppc")

(defvar outStream (make-string-output-stream))

(make-symbol "reg")
(make-symbol "imm")

(defun get-dirc-from-sym (c)
  (case c
    (reg "A")
    (imm "D")))

(defun make-ins-fmt-args (args)
  (let ((str "~{"))
  (loop for i from 0 to (- (length args) 1)
	do (setf str (if (eq i args)
			 (format nil "~A~~(~~~A~~) " str (get-dirc-from-sym (elt args i)))
			 (format nil "~A~~(~~~A~~), " str (get-dirc-from-sym (elt args i))))))
    (concatenate 'string str "~}~%")))

(defmacro def-ins (ins args)
  (list ins (list 'format 'out
		   (format nil "~A ~A"
			   ins (make-ins-fmt-args args) ) (list 'cdr 'form))) )

(defun emit-instruction (form out)
  (case (car form)
    (def-ins add (reg reg reg))
    (def-ins sub (reg reg reg))
    (def-ins mullw (reg reg reg))
    (def-ins divw (reg reg reg))
    (def-ins li (reg imm))
    (+ (format out "add ~{~(~A~), ~(~A~), ~(~A~) ~}~%" (cdr form)))
    (- (format out "subf ~{~(~A~), ~(~A~), ~(~A~) ~}~%" (cdr form)))
    (* (format out "mullw ~{~(~A~), ~(~A~), ~(~A~) ~}~%" (cdr form)))
    (/ (format out "divw ~{~(~A~), ~(~A~), ~(~A~) ~}~%" (cdr form)))
    (load (format out "li ~{~(~A~), ~D~}~%" (cdr form)))))

(defun compile-ppc-with-case (program)
    (dolist (form program)
      (emit-instruction form outStream)))

(defmacro asm-ppc (&body forms)
  (compile-ppc-with-case forms))

(defun print-usage ()
    (format t "Usage: program input output~%")
    (uiop:quit))

(if (null (uiop:command-line-arguments))
    (print-usage))

(if (< (length (uiop:command-line-arguments)) 2)
    (print-usage))

(with-open-file (outFile (elt (uiop:command-line-arguments) 1)
  :direction :output
  :if-exists :supersede
  :if-does-not-exist :create)
  (load (elt (uiop:command-line-arguments) 0))
  (write-sequence (get-output-stream-string outStream) outFile))
