;;;; package.lisp

(defpackage #:cl-npm
  (:nicknames #:npm)
  (:use #:cl #:alexandria)
  (:export
   #:*executable-path*
   #:*cache-path*
   #:load-package-json
   #:save-package-json
   #:update
   #:uninstall
   #:install
   #:npm-version
   #:webpack
   #:installed?
   #:make-node-lib))

