;;;; nodelib-util.asd

(asdf:defsystem #:nodelib-util
  :description "Support utilities for ps-lib-tool"
  :author "Ben McGunigle <bnmcgn@gmail.com>"
  :license "MIT"
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "util")))

