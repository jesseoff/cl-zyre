#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

(defpackage #:zyre
  (:use #:cl #:trivia)
  (:export #:zyre-pipe

           ;; Zyre applications
           #:zpinger
           #:zyredir
           #:zyre-daemon

           ;; ZeroMQ utils
           #:make-zsock
           #:zsock-endpoint
           #:make-zframe
           #:zframe-foreign-data
           #:send
           #:recv
           #:recv-type
           #:recv-pipe
           #:blank-zframes-pipe
           #:pipe-zsock-send

           ;; Zyre event conditions
           #:event
           #:start-event
           #:stop-event
           #:enter-event
           #:exit-event
           #:msg-carrying-event
           #:whisper-event
           #:group-event
           #:join-event
           #:leave-event
           #:leader-event
           #:shout-event
           #:other-event

           ;; Misc classes
           #:zyre-state
           #:zsock
           #:zframe
           #:zyre-idle

           ;; Zyre actions
           #:whisper
           #:shout
           #:join
           #:leave
           #:stop

           ;; Pipe utilities
           #:pipe-first
           #:pipe-cons
           #:pipe-sink
           #:pipe-sink-until
           #:pipe-sink-until-condition
           #:pipe-transform
           #:pipe-append
           #:pipe-end-before
           #:pipe-end-after
           #:pipe-last
           #:pipe-head
           #:pipe-uniq
           #:pipe-mapc
           #:pipe-filter
           #:pipe-signaler
           #:pipe-printer
           #:pipe-apply
           #:pipe-endp
           #:pipe-rest
           #:pipe-to-list
           #:list-to-pipe))
