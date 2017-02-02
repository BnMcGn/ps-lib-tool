;;;; ps-library.lisp

(in-package #:cl-npm)

;;;; Tool to create, from LISP, a library that npm will recognize.

;; ?? is there a way to see if imported lisp packages/systems constitute js
;; libraries that can be auto-imported?? -makes so that macros from js libs
;; are included without additional fuss. -reduces transparency...

;; Support for additional package.json fields?

(defparameter *default-source-path* nil)
(defparameter *default-destination-path* nil)
(defparameter *compiled-file-location* #p"js-dist/")
(defparameter *default-js-file* #p"auto-library.js")

;; things to meddle: name, files, main, dependencies?

(defun compilable-p (file)
  (and
   (member (pathname-type (pathname file)) '("lisp" "parenscript")
           :test #'equal)
   (uiop:file-exists-p
    (uiop:ensure-absolute-pathname file *default-source-path*))))

(defun destination-path (source-path &key relative)
  (make-pathname :defaults source-path :type "js"
                 :directory
                 (directory-namestring
                  (uiop:merge-pathname-directory-components
                   *compiled-file-location*
                   (if relative nil *default-destination-path*)))))

(defun process-files-field (files)
  "1st return value: 'files' as prepped for package.json.
2nd return value: input/output path pairs for compilation."
  (let ((filelist (list *compiled-file-location*))
        (pairlist))
    (loop
       for f in files
       for abs = (compilable-p f)
       for rel = (uiop:relative-pathname-p f)
       when abs
       do (let ((destpath (destination-path f)))
            (push (cons abs destpath) pairlist))
       when rel
       do (push rel filelist))
    (values (nreverse filelist) (nreverse pairlist))))

(defmacro set-paths (lib-spec &body body)
  (alexandria:once-only (lib-spec)
    `(let ((*default-source-path*
           (or *default-source-path* (asdf:system-relative-pathname
                                      (getf ,lib-spec :system-name) "")))
          (*default-destination-path*
           (or *default-destination-path*
               (asdf:system-relative-pathname
                (getf ,lib-spec :system-name) ""))))
       ,@body)))

(defun non-file-code-p (lib-spec)
  (let ((codes
         (gadgets:assoc-all :code
                            (gadgets:extract-keywords '(:code) lib-spec))))
    (apply #'concatenate 'string
           (mapcar (lambda (code) (if (functionp code)
                                      (funcall code)
                                      code))
                   codes))))

(defun main-file-p (lib-spec)
  "Returns relative name of the file that will be 'main' in the final
package.json"
  (alexandria:if-let
      ((main (getf lib-spec :main)))
    (if (compilable-p main)
        (destination-path main :relative t)
        main)
    (if (non-file-code-p lib-spec)
        (uiop:merge-pathnames* *default-js-file* *compiled-file-location*)
        nil)))

(defun make-ps-library (lib-spec)
  (set-paths lib-spec
    (multiple-value-bind (files-list compilables)
        (process-files-field (getf lib-spec :files))
      (let ((non-file (non-file-code-p lib-spec))
            (main-file (main-file-p lib-spec)))
        ;;compile the files
        (loop for (src . dest) in compilables
           do
             (with-open-file (ps:*parenscript-stream*
                              dest :direction :output
                              :if-exists :overwrite)
                              (parenscript:ps-compile-file src)))
        ;;save the loose code
        (when (length non-file)
          (with-open-file (s (reduce #'uiop:merge-pathnames*
                                     (list *default-js-file*
                                           *compiled-file-location*
                                           *default-destination-path*))
                             :direction :output
                             :if-exists :overwrite)
            (write-string non-file s)))
        ;;generate the package.json
        (let ((misc-fields
               (nth-value
                1
                (gadgets:extract-keywords
                 '(:files :main :code :system-name) lib-spec))))
          (with-open-file (s (uiop:merge-pathnames*
                              "package.json" *default-destination-path*)
                             :direction :output
                             :if-exists :overwrite)
            (json:encode-json-plist
             (list*
              :files files-list
              :main main-file
              misc-fields)
             s)))))))
