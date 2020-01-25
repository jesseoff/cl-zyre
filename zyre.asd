#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi-grovel))

(asdf:defsystem #:zyre
  :description "Zyre is a ZeroMQ-based network protocol for clusters and service discovery."
  :depends-on (:cffi :trivial-garbage :trivia :osicat :uiop)
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
  :author "Jesse Off <jesseoff@me.com>"
  :license "MIT"
  :entry-point "zyre::zpinger"
  :serial t
  :components ((:file "package")
               (:file "zpinger")))

(asdf:defsystem #:zyre/zyredir
  :description "Sample app that maintains a /tmp/zyre/ of JSON files of online peers."
  :depends-on (:zyre :cl-json)
  :author "Jesse Off <jesseoff@me.com>"
  :license "MIT"
  :entry-point "zyre::zyredir"
  :serial t
  :components ((:file "package")
               (:file "zyredir")))

(asdf:defsystem #:zyre/tock-server
  :description "Sample app that shouts its time once/sec to group tick-tock."
  :depends-on (:zyre :local-time-duration :local-time)
  :author "Jesse Off <jesseoff@me.com>"
  :license "MIT"
  :entry-point "zyre::tock-server"
  :serial t
  :components ((:file "package")
               (:file "tock")))

(asdf:defsystem #:zyre/tock-client
  :description "Sample app that compares its system time against shouts to group tick-tock."
  :depends-on (:zyre :local-time-duration :local-time)
  :author "Jesse Off <jesseoff@me.com>"
  :license "MIT"
  :entry-point "zyre::tock-client"
  :serial t
  :components ((:file "package")
               (:file "tock")))

