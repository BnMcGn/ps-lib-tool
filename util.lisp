;;;; package.lisp

(defpackage #:nodelib-util
  (:use #:cl #:parenscript)
  (:export
   #:ps-load))

(in-package #:nodelib-util)

(defpsmacro ps-load (ps-file-path)
  (uiop:with-current-directory ((pathname-directory ps-file-path))
    (with-open-file (s ps-file-path)
      (list*
       'progn
       (loop for form = (read s nil :end)
             until (eq form :end)
             collecting form)))))
