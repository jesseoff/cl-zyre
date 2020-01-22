#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi-grovel))

(asdf:defsystem #:zyre
  :description "Zyre is a ZeroMQ-based network protocol for clusters and service discovery."
  :depends-on (:cffi :trivial-garbage :trivia)
  :author "Jesse Off <jesseoff@me.com>"
  :license "MIT"
  :version "1"
  :serial t
  :components ((:file "package")
               (:file "fifo")
               (:file "pipe")
               (:cffi-grovel-file "grovel")
               (:file "zyre")))

(asdf:defsystem #:zyre/zpinger
  :description "Lisp equivalent of the Zyre C library sample application of the same name."
  :depends-on (:zyre :log4cl :cl-ppcre)
  :license "MIT"
  :entry-point "zyre::zpinger"
  :serial t
  :components ((:file "package")
               (:file "zpinger")))

(asdf:defsystem #:zyre/zyredir
  :description "Sample app that maintains a /tmp/zyre/ of JSON files of online peers."
  :depends-on (:zyre :cl-json)
  :license "MIT"
  :entry-point "zyre::zyredir"
  :serial t
  :components ((:file "package")
               (:file "zyredir")))

