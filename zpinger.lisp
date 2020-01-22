#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:zyre)

(defun zpinger ()
  "Sends Hello messages to any peer who enters, and shouts Hello to any peer who whispers back.
This is just like the sample app contained in the zyre C library. Log messages are also almost
identical to the C library reference application."
  (labels
      ((handler (x)
         (match x
           ((start-event (uuid u) (name n))
            (log:info "Create Zyre node, uuid=~a, name=~a" u n)
            nil)
           ((exit-event (name n))
            (log:info "[~a] peer exited" n)
            nil)
           ((enter-event (name n) (peer-addr a))
            (whisper x "Hello")
            (log:info "[~a] peer entered, ip=~a" n
                      (ppcre:scan-to-strings "[0-9\.]+" a))
            nil)
           ((shout-event (event-msg "Hello") (name n) (group g))
            (log:info "[~a](~a) received ping (SHOUT)" n g)
            t)
           ((whisper-event (event-msg "Hello") (name n))
            (shout x "Hello")
            (log:info "[~a] received ping (WHISPER)" n)
            t)
           (otherwise nil))))
    (let ((pipe (zyre-pipe :group "GLOBAL" :name (machine-instance))))
      (pipe-mapc #'handler pipe))))
