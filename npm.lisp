;;;; cl-npm.lisp

(in-package #:cl-npm)

;;; "cl-npm" goes here. Hacks and cwdebuggingglory await!

(defvar *npm-executable-path* #p"/usr/bin/npm")
(defvar *node-executable-path* #p"/usr/bin/nodejs")
(defvar *webpack-executable-path* nil)
(defvar *cache-path* (asdf:system-relative-pathname
                      'cl-npm "cache" :type :directory))

(defun extend-pathname (path &rest extensions)
  (let ((exts
         (apply
          #'concatenate 'list
          (mapcar (lambda (x)
                    (if (stringp x)
                        (list x)
                        (let ((pd (pathname-directory x)))
                          (unless (eq (car pd) :relative)
                            (error "Extension must not be an absolute path"))
                          (cdr pd))))
                  extensions))))
    (make-pathname :defaults path
                   :directory (append (pathname-directory path) exts))))

(defun package-json-path ()
  (merge-pathnames *cache-path* "package.json"))

(defun npm-executable-path ()
  (unless (uiop:file-exists-p *npm-executable-path*)
    (error
     (format nil "Npm not found at ~a. Please install npm or specify an executable with the *npm-executable-path* variable." *npm-executable-path*)))
  *npm-executable-path*)

(defun node-executable-path ()
  (unless (uiop:file-exists-p *node-executable-path*)
    (error
     (format nil "Nodejs not found at ~a. Please install node or specify an executable with the *node-executable-path* variable." *node-executable-path*)))
  *node-executable-path*)

(defun webpack-executable-path ()
  (if *webpack-executable-path* *webpack-executable-path*
      (make-pathname :defaults (extend-pathname *cache-path* "node_modules"
                                                "webpack" "bin")
                     :name "webpack.js")))

(defun load-package-json ()
  "Load and parse the packages.json file from the working cache."
  (when (uiop:file-exists-p (package-json-path))
    (json:decode-json-from-source (package-json-path))))

(defun save-package-json (data)
  "Saves the supplied data in the cache directory's package.json file. Uses cl-json to convert to json format. See npm documentation for file format."
  (with-open-file (fh (package-json-path)
                      :direction :output :if-exists :overwrite
                      :if-does-not-exist :create)
    (json:encode-json data fh)))

(defun npm-command (&rest command)
  (uiop:with-current-directory (*cache-path*)
    (uiop/run-program:run-program
     (list* (npm-executable-path) command)
     :output :lines)))

(defun npm-version ()
  "Returns the version of the npm binary in use."
  (car (npm-command "-v")))

(defun update ()
  "Updates all npm packages in the cache."
  (npm-command "update"))

(defun install (package-name &key global)
  "Installs named package in the cache directory. If global keyword is set attempts to install package globally."
  (apply #'npm-command `("install" ,@(when global "-g") ,package-name)))

(defun uninstall (package-name &key save global)
  (apply #'npm-command
         `("uninstall"
           ,@(when save "--save") ,@(when global "-g") ,package-name)))

(defun initialize-webpack ()
  (let ((webpack (webpack-executable-path)))
    (unless (uiop:file-exists-p webpack)
      (if *webpack-executable-path*
          (error (format nil
                         "webpack not found at ~a. please install webpack, set the *webpack-executable-path* variable to nil, or specify another location in *webpack-executable-path*."
                         *webpack-executable-path*))
          (install "webpack")))))

(defun webpack (target &rest sources)
  (initialize-webpack)
  (uiop:with-current-directory (*cache-path*)
    (uiop:run-program
     (list*
      (webpack-executable-path)
      target
      sources))))
