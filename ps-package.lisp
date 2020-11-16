;;;; ps-package

(in-package #:ps-lib-tool)

(defvar *ps-packages* (make-hash-table :test 'eq))

(defmacro def-ps-package (pname &key ps-requirements js-requirements init-code export code ps-files
                                  host-package)
  "Define a parenscript package. Ps-packages are used to bundle parenscript code and specify dependencies. Dependencies can be other ps-packages or npm libraries. Ps-packages can be built into javascript programmatically or can be exported as npm libraries."
  (declare (symbol pname))
  `(save-ps-package
    ',pname
    :ps-requirements ,ps-requirements :js-requirements ,js-requirements :init-code ',init-code
    :export ,export :code ,code :ps-files ,ps-files :host-package ,host-package))

(defmacro get-package (name)
  `(gethash (alexandria:make-keyword (gadgets:to-uppercase ,name)) *ps-packages*))

(defun save-ps-package (name &key ps-requirements js-requirements init-code export code ps-files
                               host-package)
  (setf (get-package name)
        (list
         :ps-requirements ps-requirements
         :js-requirements (check-js-requirements js-requirements)
         :init-code init-code
         ;;FIXME: export might be obsolete. Use a node lib.
         :export export
         :code code
         :ps-files (check-ps-files ps-files)
         :host-package or (host-package *package*))))

(defun check-js-requirements (imps)
  (unless
      (every (lambda (x) (or (stringp x) (and (listp x) (every #'stringp x))))
             imps)
    (error "Invalid item in js-requirements section"))
  imps)

(defun check-ps-files (files)
  (when (notevery #'probe-file files)
    (error "Can't find specified ps-file"))
  files)

(defun ensure-system-loaded (system)
  "If the system exists and isn't loaded, load it."
  (or
   (member system (asdf:already-loaded-systems) :test #'string-equal)
   (handler-case
       (progn
         (asdf:find-system system)
         (ql:quickload system))
     (asdf:missing-component (mc) (declare (ignore mc)) nil))))

(defun find-all-required-ps-packages (&rest required-syms)
  "Recursively find all of the ps-packages that will be needed by the specified package(s). Will NOT search for packages in the depends-on section of .asd files. Dependencies must be explicitly stated in the ps-requirements section of the def-ps-package form. Includes requiring packages."
  (let ((stor (make-hash-table))
        (work (mapcar (alexandria:compose #'alexandria:make-keyword #'gadgets:to-uppercase)
                      required-syms))
        (new-work nil))
    (loop while work
          do (progn
               (dolist (item work)
                 (when item
                   ;; Requested ps-packages might not (yet) have a ps-package declaration
                   (unless (or (gethash item *ps-packages*)
                               (ensure-system-loaded item))
                     (error "PS library not found"))
                   (unless (gethash item stor)
                     (push (getf (gethash item *ps-packages*) :ps-requirements nil) new-work)
                     (setf (gethash item stor) (gethash item *ps-packages*)))))
               (setf work (mapcar (alexandria:compose #'alexandria:make-keyword #'gadgets:to-uppercase)
                                  (apply #'append new-work)))
               (setf new-work nil)))
    stor))

(defun get-all-js-requirements (&rest ps-packages)
  (let ((accum nil))
    (maphash
     (lambda (key pack)
       (declare (ignore key))
       (push (getf pack :js-requirements) accum))
     (apply #'find-all-required-ps-packages ps-packages))
    (apply #'append (nreverse accum))))

(defun get-init-code-blocks (&rest ps-packages)
  (let ((accum nil))
    (maphash (lambda (key pack)
              (declare (ignore key))
              (push (getf pack :init-code) accum))
             (apply #'find-all-required-ps-packages ps-packages))
    (nreverse accum)))

(defun make-init-preface (ps-packages)
  "Make sure that the required structures exist for the init code."
  (let* ((allpacks (apply #'find-all-required-ps-packages ps-packages))
         (init-syms
           (cl-utilities:collecting
             (gadgets:do-hash-table (k pack allpacks)
               (when (and pack (getf pack :init-code))
                 (cl-utilities:collect k))))))
    ;; Top level init must be in top level code, so we don't wrap in progn. Must compile each item in
    ;; this list, then concatenate resulting js strings.
    `((defparameter ,*import-manager-location*
        (if ,*import-manager-location*
            ,*import-manager-location*
            (new -object)))
      ,@(cl-utilities:collecting
          (dolist (isym init-syms)
            (cl-utilities:collect
                `(unless (ps:chain ,*import-manager-location* (has-own-property ,isym))
                   (setf (ps:@ ,*import-manager-location* ,isym) (new -object)))))))))

(defun get-init-code (&rest ps-packages)
  (append
   (make-init-preface ps-packages)
   ;;Strip out (ps ) wrappers
   (gadgets:flatten-1
    (mapcar (lambda (code)
              (if (eq (car code) 'ps:ps) (cdr code) (list code)))
            (remove-if-not #'identity (apply #'get-init-code-blocks ps-packages))))))

(defun get-init-js (&rest ps-packages)
  (apply #'gadgets:strcat (mapcar (lambda (x) (ps:ps* x)) (apply #'get-init-code ps-packages))))

(defun get-code (ps-package)
  (let* ((pack (or (get-package ps-package)
                   (error "PS package not found")))
         (code (or (getf pack :code) ""))
         ;;Make macros from used systems available to .ps files
         (codes (let ((*package* (getf pack :host-package)))
                  (mapcar #'ps:ps-compile-file (getf pack :ps-files))))
         (codes (remove-if-not (lambda (x) x) codes))
         (init (get-init-js ps-package)))
    (when (or (gadgets:not-empty code)
               (some #'gadgets:not-empty codes))
      (apply #'concatenate 'string init code (remove-if-not (lambda (x) x) codes)))))

(defun strip-version-string (vstring)
  (nth-value 1 (gadgets:part-on-true (lambda (x) (<= (char-int #\0) (char-int x) (char-int #\9)))
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





