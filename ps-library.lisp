;;;; ps-library

(in-package #:ps-lib-tool)

(defvar *ps-packages* (make-hash-table :test 'eq))

(defmacro def-ps-package (pname &key ps-imports js-imports code export)
  "Define a parenscript package. Ps-packages are used to bundle parenscript code and specify dependencies. Dependencies can be other ps-packages or npm libraries. Ps-packages can be built into javascript programmatically or can be exported as npm libraries."
  (declare (symbol pname))
  `(progn
     (save-ps-package ',pname
                      :ps-imports ,ps-imports :js-imports ,js-imports :code ',code :export ,export)))

(defun collect-names (code)
  (let ((names nil))
    (dolist (itm code)
      (when (and (listp itm) (eq 'paren6:import (car itm)))
        (dolist (name (second itm))
          (push (if (listp name) (second name) name) names))))
    (nreverse names)))

(defun save-ps-package (name &key ps-imports js-imports code export)
  (setf (gethash (gadgets:keywordize name) *ps-packages*)
        (list
         :ps-imports ps-imports
         :js-imports (check-js-imports js-imports)
         :code code
         ;;FIXME: export might be obsolete. Use a node lib.
         :export export)))

(defun check-js-imports (imps)
  (unless
      (every (lambda (x) (or (stringp x) (and (listp x) (every #'stringp x))))
             imps)
    (error "Invalid item in js-imports section"))
  imps)

(defun system-loaded-p (sys)
  (member sys))

(defun find-all-required-ps-packages (&rest required-syms)
  "Recursively find all of the ps-packages that will be needed by the specified package(s). Will NOT search for packages in the depends-on section of .asd files. Dependencies must be explicitly stated in the ps-imports section of the def-ps-package form. Includes requiring packages."
  (let ((stor (make-hash-table))
        (work (mapcar #'gadgets:keywordize required-syms))
        (new-work nil))
    (loop while work
          do (progn
               (dolist (item work)
                 (when item
                   ;; Requested ps-packages might not (yet) have a ps-package declaration
                   (unless (or (gethash item *ps-packages*)
                               (member item (asdf:already-loaded-systems)
                                       :test #'gadgets:string-equal-caseless))
                     (error "PS library not found"))
                   (unless (gethash item stor)
                     (push (getf (gethash item *ps-packages*) :ps-imports nil) new-work)
                     (setf (gethash item stor) (gethash item *ps-packages*)))))
               (setf work (mapcar #'gadgets:keywordize (apply #'append new-work)))
               (setf new-work nil)))
    stor))

(defun get-all-js-requirements (&rest ps-packages)
  (let ((accum nil))
    (maphash
     (lambda (key pack)
       (declare (ignore key))
       (push (getf pack :js-imports) accum))
     (apply #'find-all-required-ps-packages ps-packages))
    (apply #'append (nreverse accum))))

(defun get-code-blocks (&rest ps-packages)
  (let ((accum nil))
    (maphash (lambda (key pack)
              (declare (ignore key))
              (push (getf pack :code) accum))
             (apply #'find-all-required-ps-packages ps-packages))
    (nreverse accum)))

(defun get-bootstrap-code (&rest ps-packages)
  (cons
   'progn
   ;;Strip out (ps ) wrappers
   (mapcar (lambda (code) (if (eq (car code) 'ps:ps) (cdr code) code))
           (apply #'get-code-blocks ps-packages))))

(defun strip-version-string (vstring)
  (nth-value 1 (gadgets:divide-on-true (lambda (x) (<= (char-int #\0) (char-int x) (char-int #\9)))
                                       vstring)))

(defun compare-versions (ver1 ver2)
  "Returns the larger"
  (if ver1
      (if ver2
          (if (semver:version<=
               (semver:read-version-from-string (strip-version-string ver1))
               (semver:read-version-from-string (strip-version-string ver2)))
              ver2 ver1)
          ver1)
      ver2))

(defun merge-js-requirements (reqs)
  "Requirements come in as a list with possible duplicates and/or different version specifications. Reduce to list of same first appearance order with highest requested version for each library."
  (let ((stor (make-hash-table :test #'equal))
        (accum))
    (dolist (itm reqs)
      (let ((key (if (listp itm) (car itm) itm))
            (version (if (listp itm) (second itm) nil)))
        (when version (assert (stringp version)))
        (push key accum)
        (setf (gethash key stor)
              (if-let ((val (gethash key stor)))
                (compare-versions val version)
                version))))
    (mapcar
     (lambda (key)
       (let ((val (gethash key stor)))
         (if val (list key val) key)))
     (gadgets:ordered-unique (nreverse accum)))))





