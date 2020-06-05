(in-package #:ps-lib-tool)

(defun handle-keyed-list (klist)
  (cond
    ((hash-table-p klist) klist)
    ((not (listp klist)) (error "Section should be a keyed object"))
    ((every #'consp klist) (hu:alist->hash klist))
    ;;We hope it is a plist. Maybe should check?
    (t (hu:plist->hash klist))))

(defun create-package.json-data (&key name description version license repository main
                                   scripts dev-dependencies keywords)
  (let ((res nil))
    (when name (push :name res) (push name res))
    (when description (push :description res) (push description res))
    (when version (push :version res) (push version res))
    (when license (push :license res) (push license res))
    (when main (push :main res) (push main res))
    (when repository (push :repository res) (push (handle-keyed-list repository) res))
    (when scripts (push :scripts res) (push (handle-keyed-list scripts) res))
    (when dev-dependencies (push :dev-dependencies res) (push (handle-keyed-list dev-dependencies) res))
    (when keywords (push :keywords res) (push keywords res))
    (nreverse res)))

(defun safe-to-write? (location)
  (when (uiop:file-exists-p location)
    (error "Project destination already exists as a file"))
  (when (and (uiop:directory-exists-p location)
             (some (lambda (x) (probe-file (merge-pathnames location x)))
                   '("README.md" "package.json" "src" "node_modules" "build.sh")))
    (error "Project directory already contains files")))

(defun write-readme (name location
                     &key
                       license
                       description)
  (with-open-file (s (merge-pathnames location "README.md") :direction :output
                                                            :if-does-not-exist :create)
    (format s "# ~:(~a~) ~%~% ~a ~%~% ### License ~%~% ~a ~&"
            name (or description "[add a description]") (or license "[specify license]"))))

(defun write-build.sh (location filenames)
  (with-open-file (s (merge-pathnames location "build.sh") :direction :output :if-does-not-exist :create)
    (format s
            "#!/bin/sh

cd src

~{../node_modules/sigil-cli/sigil --eval \"(progn (ql:quickload 'nodelib-util) (use-package :nodelib-util))\" ~a.parenscript > ../~:*~a.js~%~}

#Add source files here
# ../node_modules/sigil-cli/sigil --eval \"(progn (ql:quickload 'nodelib-util) (use-package :nodelib-util))\" your_file.parenscript > ../your_file.js~&" filenames)))

(defun write-sourcefile (location name)
  (let ((path (make-pathname :directory (append (pathname-directory location) '("src"))
                             :name (format nil "~a.parenscript" name))))
    (ensure-directories-exist path)
    (with-open-file (s path
                     :direction :output :if-does-not-exist :create)
      (format s ";;; ~a.parenscript~%~%(ps-load \"resources.parenscript\")" name))))

;;;FIXME: Rebuilding the resources file must be externally available.
(defun write-resources.parenscript (location name code dependencies)
  (with-open-file (s (make-pathname :directory (append (pathname-directory location) '("src"))
                                    :name "resources.parenscript")
                     :direction :output :if-does-not-exist :create)
    (format s ";;; Resources for ~a~%~%
;;;~% (lisp
 (progn
   (ql:quickload \"paren6\")
   (use-package :paren6 :ps)))

;;; Suggested import clauses for dependencies. Uncomment to use.

~{;;; (import ((:default ~a)) ~:*\"~a\")~%~}

;;; Execute code blocks from parenscript library dependencies: ~%

~s

;;;
" name dependencies (apply #'get-bootstrap-code code))))

(defun write-nodelib-project-to-location (location name data &key license description main)
  (safe-to-write? location)
  (ensure-directories-exist location)
  (write-build.sh location (list (or main name)))
  (write-readme name location :license license :description description)
  (write-sourcefile location (or main name))


  (with-open-file (out (merge-pathnames location "package.json") :direction :output
                       :if-does-not-exist :create)
    (with-input-from-string (s (json:encode-json-plist-to-string data))
      (pretty-print-json s out))))

(defun make-node-lib (location &key name description version license repository main
                                scripts dev-dependencies dependencies ps-dependencies keywords)
  (let ((name (or name (pathname-name location) (car (last (pathname-directory location)))))
        (nodereqs (apply #'get-all-js-requirements ps-dependencies))
        (deps (check-js-imports dependencies)))
    (write-nodelib-project-to-location
     location
     name
     (create-package.json-data
      :name name
      :description description
      :version (or version "0.0.1")
      :license license
      :main (or main (format nil "~a.js" name))
      :repository repository
      :scripts (list* "build" "sh build.sh" scripts)
      :dev-dependencies
      (list* "sigil-cli" "^1.0.6" dev-dependencies)
      :keywords keywords)
     :license license
     :description description
     :main main)
    ;;This writes to the package.json file created in step above
    (dolist (dep (append deps nodereqs))
      (install-save location (if (listp dep) (car dep) dep) :version (when (listp dep) (second dep))))
    (write-resources.parenscript
     location name (mapcar #'gadgets:keywordize ps-dependencies) deps)))

;;; JSON pretty printer

(defun indent-newline (outstream depth indent)
  (write-char #\Newline outstream)
  (if (integerp indent)
      (dotimes (i (* indent depth))
        (write-char #\space outstream))
      (dotimes (i depth)
        (write-char indent outstream))))

(defun pretty-print-json (in out &key (indent 2))
  (macrolet ((end-loop (&rest params)
               `(return-from pretty-print-json ,@params)))
    (loop while (not done)
         with done = nil
         with depth = 0
         for char = (read-char in nil :end nil)
          do (block loop-top
              (when (eq :end char)
                (end-loop))
              (write-char char out)
              (cond
                ((eq #\" char) (advance-to-end-of-string in out #\"))
                ((eq #\, char)
                 (indent-newline out depth indent))
                ((member char '(#\{ #\( #\[))
                 (incf depth)
                 (indent-newline out depth indent))
                ((member char '(#\} #\) #\]))
                 (decf depth)))))))

(defun advance-to-end-of-string (in out termchar)
  (loop while (not done)
        with done = nil
        with escape = nil
        for char = (read-char in nil :end nil)
        do (if (eq :end char)
               (return)
               (progn
                 (write-char char out)
                 (if escape
                     (setf escape nil)
                     (cond
                       ((eq #\\ char) (setf escape t))
                       ((eq char termchar) (return))))))))
