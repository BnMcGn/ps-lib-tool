;;;; ps-library

(in-package #:cl-npm)

(defvar *ps-packages* (make-hash-table))

(defun def-ps-package (name &key use npm-libraries code export)
  "Define a parenscript package. Ps-packages are used to bundle parenscript code and specify dependencies. Dependencies can be other ps-packages or npm libraries. Ps-packages can be built into javascript programmatically or can be exported as npm libraries."

  (setf (gethash name *ps-packages*)
        (list
         :use use
         :npm-libraries npm-libraries
         :code code
         :export export)))

(defun merge-ps-packages (packages)
  ;;FIXME: what do we do with use?
  (let ((npm nil)
        (code nil)
        (export nil))
    (dolist (pack packages)
      ;(setf use (concatenate 'list use (getf pack :use)))
      (setf npm (concatenate 'list npm (getf pack :npm-libraries)))
      (setf export (concatenate 'list export (getf pack :export)))
      (setf code (if code
                     (if (getf pack :code)
                         (lambda ()
                           (concatenate 'string (funcall code) (funcall (getf pack :code))))
                         code)
                     (getf pack :code))))
    (list :use nil
          :npm-libraries (gadgets:ordered-unique npm :test #'string-equal)
          :code code
          :export (gadgets:ordered-unique export :test #'string-equal))))
