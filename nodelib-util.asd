;;;; cl-npm.asd

(asdf:defsystem #:nodelib-util
  :description "Support utilities for nodelib"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "util")))

