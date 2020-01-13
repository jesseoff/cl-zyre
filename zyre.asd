#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem #:zyre
  :description "Zyre is a ZeroMQ-based network protocol for clusters and service discovery."
  :depends-on ( :cffi :trivial-garbage :bordeaux-threads :trivia :log4cl :cl-json :cl-ppcre )
  :author "Jesse Off <jesseoff@me.com>"
  :license  "MIT"
  :version "1"
  :serial t
  :components ((:file "package")
               (:file "fifo")
               (:file "pipe")
               (cffi-grovel:grovel-file "grovel")
               (:file "zyre")))
