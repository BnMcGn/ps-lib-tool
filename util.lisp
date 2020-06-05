;;;; package.lisp

(defpackage #:nodelib-util
  (:use #:cl #:parenscript)
  (:export
   #:ps-load
   #:ps-load-expr))

(in-package #:nodelib-util)

(defpsmacro ps-load (ps-file-path)
  (uiop:with-current-directory ((pathname-directory ps-file-path))
    (with-open-file (s ps-file-path)
      (list*
       'progn
       (loop for form = (read s nil :end)
             until (eq form :end)
             collecting form)))))

(defpsmacro ps-load-expr (expr)
  "Load as ps code the results of lisp expression expr"
  (eval (if (eq (car expr) 'ps:ps) (cdr expr) expr)))

