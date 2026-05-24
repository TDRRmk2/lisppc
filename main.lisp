(require "uiop")

(defvar outStream (make-string-output-stream))

(defun emit-instruction (form out)
  (case (car form)
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
