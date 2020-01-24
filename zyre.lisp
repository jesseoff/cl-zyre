#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

;; XXX: zmsg class and functions will need cleanup before exposing publicly.
;; Some take foreign pointers, some take zmsg instances, many functions missing.

(in-package #:zyre)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library libzyre
    (:darwin (:or "libzyre.dylib"
                  "libzyre.2.dylib"))
    (:unix (:or "libzyre.so"
                "/usr/lib/libzyre.so"
                "/usr/local/lib/libzyre.so"))
    (:windows (:or "libzyre.dll"
                   "zyre.dll"))
    (t (:default "libzyre")))

  (define-condition event () ((uuid :type string :initarg :uuid :reader uuid)
                      (name :type string :initarg :name :reader name)
                      (zyre :initarg :zyre :accessor event-zyre)))
  (define-condition start-event (event) ())
  (define-condition stop-event (event) ())
  (define-condition enter-event (event) ((headers :initarg :headers :reader headers)
                                 (peer-addr :type string :initarg :peer-addr :reader peer-addr)))
  (define-condition exit-event (event) ())
  (define-condition msg-carrying-event () ((msg :initarg :msg :type string :accessor event-msg)))
  (define-condition whisper-event (event msg-carrying-event) ())
  (define-condition group-event (event) ((group :type string :initarg :group :reader group)))
  (define-condition join-event (group-event) ())
  (define-condition leave-event (group-event) ())
  (define-condition leader-event (group-event) ())
  (define-condition shout-event (group-event msg-carrying-event) ())
  (define-condition other-event (event)
    ((event-type :type string :initarg :event-type :reader event-type)))

  (defclass zyre-state ()
    ((enter-events-by-uuid :initform (make-hash-table)
                           :type hash-table
                           :accessor enter-events-by-uuid)
     (uuid :type string :initarg :uuid :reader uuid)
     (name :type string :initarg :name :reader name)
     (group :accessor group :initform nil)
     (stopped :type boolean :initform nil :accessor stopped)
     (raw-zpoller :initarg :raw-zpoller :type cffi:foreign-pointer :reader raw-zpoller)
     (raw-zyre :initarg :raw-zyre :type cffi:foreign-pointer :reader raw-zyre)))

  (defclass zsock ()
    ((raw-zsock :type cffi:foreign-pointer :reader raw-zsock :initarg :raw-zsock)
     (zsock-type :type keyword :reader zsock-type :initarg :zsock-type)))

  (defclass zmsg ()
    ((raw-zframe :initarg :raw-zmsg :type cffi:foreign-pointer :reader raw-zmsg)))

  (defclass zframe ()
    ((raw-zframe :initarg :raw-zframe :type cffi:foreign-pointer :reader raw-zframe))))

(cffi:use-foreign-library libzyre)

(cffi:defcfun ("zyre_new" zyre-new) :pointer
  (name :string))

(cffi:defcfun ("zyre_destroy" %zyre-destroy) :void
  (zyre-** :pointer))

(cffi:defcfun ("zyre_set_verbose" zyre-set-verbose) :void
  (self :pointer))

(cffi:defcfun ("zsys_shutdown" zsys-shutdown) :void)

(cffi:defcfun ("zyre_uuid" zyre-uuid) :string
  (self :pointer))

(cffi:defcfun ("zyre_name" zyre-name) :string
  (self :pointer))

(cffi:defcfun ("zyre_start" zyre-start) :int
  (self :pointer))

(cffi:defcfun ("zyre_stop" zyre-stop) :void
  (self :pointer))

(cffi:defcfun ("zyre_join" zyre-join) :int
  (self :pointer)
  (group :string))

(cffi:defcfun ("zyre_leave" zyre-leave) :int
  (self :pointer)
  (group :string))

(cffi:defcfun ("zyre_shouts" zyre-shouts) :int
  (self :pointer)
  (group :string)
  (format :string)
  &rest)

(cffi:defcfun ("zyre_whispers" zyre-whispers) :int
  (self :pointer)
  (peer :string)
  (format :string)
  &rest)

(cffi:defcfun ("zyre_set_header" zyre-set-header) :int
  (self :pointer)
  (header :string)
  (printf-format :string)
  &rest)

(cffi:defcfun ("zyre_peers" %zyre-peers) :pointer
  (self :pointer))

(cffi:defcfun ("zlist_pop" zlist-pop) :pointer
  (self :pointer))

(cffi:defcfun ("zyre_set_interface" zyre-set-interface) :void
  (self :pointer)
  (interface :string))

(cffi:defcfun ("zlist_destroy" %zlist-destroy) :void
  (zlist-** :pointer))

(cffi:defcfun ("zstr_free" %zstr-free) :void
  (str :pointer))

(defun zstr-free (str)
  (declare (type cffi:foreign-pointer str))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-aref ptr :pointer) str)
    (%zstr-free ptr)))

(defun map-zlist (zlist)
  (declare (type cffi:foreign-pointer zlist))
  (labels
      ((zstr (x)
         (prog1 (cffi:foreign-string-to-lisp x)
           (zstr-free x)))
       (listify ()
         (let ((x (zlist-pop zlist)))
           (if (not (cffi:null-pointer-p x))
               (cons (zstr x) (listify))
               nil))))
    (prog1
        (listify)
      (zlist-destroy zlist))))

(defun zyre-peers (zyre)
  (declare (type cffi:foreign-pointer zyre))
  (map-zlist (%zyre-peers zyre)))

(defun zlist-destroy (zlist)
  (declare (type cffi:foreign-pointer zlist))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-aref ptr :pointer) zlist)
    (%zlist-destroy ptr)))

(cffi:defcfun ("zyre_peer_groups" %zyre-peer-groups) :pointer
  (self :pointer))

(defun zyre-peer-groups (zyre)
  (declare (type cffi:foreign-pointer zyre))
  (map-zlist (%zyre-peer-groups zyre)))

(cffi:defcfun ("zyre_peers_by_group" %zyre-peers-by-group) :pointer
  (self :pointer)
  (group :string))

(defun zyre-peers-by-group (zyre group)
  (declare (type cffi:foreign-pointer zyre) (type base-string group))
  (map-zlist (%zyre-peers-by-group zyre group)))

(cffi:defcfun ("zyre_peer_address" %zyre-peer-address) :pointer
  (self :pointer)
  (peer :string))

(defun zyre-peer-address (zyre peer)
  (declare (type cffi:foreign-pointer zyre) (type base-string peer))
  (let ((x (%zyre-peer-address zyre peer)))
    (prog1 (cffi:foreign-string-to-lisp x)
      (zstr-free x))))

(cffi:defcfun ("zyre_peer_header_value" %zyre-peer-header-value) :pointer
  (self :pointer)
  (peer :string)
  (name :string))

(defun zyre-peer-header-value (zyre peer name)
  (declare (type cffi:foreign-pointer zyre) (type base-string peer name))
  (let ((x (%zyre-peer-header-value zyre peer name)))
    (prog1 (cffi:foreign-string-to-lisp x)
      (when (not (cffi:null-pointer-p x)) (zstr-free x)))))

(cffi:defcfun ("zyre_set_port" zyre-set-port) :void
  (self :pointer)
  (port :int))

(cffi:defcfun ("zyre_print" zyre-print) :void
  (self :pointer))

(cffi:defcfun ("zyre_own_groups" %zyre-own-groups) :pointer
  (self :pointer))

(defun zyre-own-groups (zyre)
  (declare (type cffi:foreign-pointer zyre))
  (map-zlist (%zyre-own-groups zyre)))

(defun zyre-destroy (zyre)
  (declare (type cffi:foreign-pointer zyre))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-aref ptr :pointer) zyre)
    (%zyre-destroy ptr)))

(cffi:defcfun ("zyre_version" zyre-version) :uint64)

(cffi:defcfun ("zyre_recv" %zyre-recv) :pointer
  (zyre :pointer))

(cffi:defcfun ("zyre_socket" %zyre-socket) :pointer
  (zyre :pointer))

(cffi:defcfun ("zmsg_recv" %zmsg-recv) :pointer
  (socket :pointer))

(cffi:defcfun ("zmsg_popstr" %zmsg-popstr) :pointer
  (zmsg :pointer))

(defun zmsg-popstr (zmsg)
  (declare (type (or zmsg cffi:foreign-pointer) zmsg))
  (etypecase zmsg
    (zmsg (zmsg-popstr (raw-zmsg zmsg)))
    (cffi:foreign-pointer
     (let ((x (%zmsg-popstr zmsg)))
       (prog1 (cffi:foreign-string-to-lisp x)
         (zstr-free x))))))

(cffi:defcfun ("zmsg_pop" %zmsg-pop) :pointer
  (zmsg :pointer))

(defun zmsg-pop (zmsg)
  (declare (type (or zmsg cffi:foreign-pointer) zmsg))
  (etypecase zmsg
    (zmsg (%zmsg-pop (raw-zmsg zmsg)))
    (cffi:foreign-pointer (%zmsg-pop zmsg))))

(cffi:defcfun ("zhash_unpack" zhash-unpack) :pointer
  (zframe :pointer))

(cffi:defcfun ("zframe_destroy" %zframe-destroy) :void
  (zframe :pointer))

(defun zframe-destroy (zframe)
  (declare (type (or zframe cffi:foreign-pointer) zframe))
  (etypecase zframe
    (zframe (zframe-destroy (raw-zframe zframe))
     (tg:cancel-finalization zframe))
    (cffi:foreign-pointer
     (cffi:with-foreign-object (ptr :pointer)
       (setf (cffi:mem-aref ptr :pointer) zframe)
       (%zframe-destroy ptr)))))

(cffi:defcfun ("zmsg_destroy" %zmsg-destroy) :void
  (zmsg :pointer))

(defun zmsg-destroy (zmsg)
  (declare (type (or zmsg cffi:foreign-pointer) zmsg))
  (etypecase zmsg
    (zmsg (zmsg-destroy (raw-zmsg zmsg))
     (tg:cancel-finalization zmsg))
    (cffi:foreign-pointer
     (cffi:with-foreign-object (ptr :pointer)
       (setf (cffi:mem-aref ptr :pointer) zmsg)
       (%zmsg-destroy ptr)))))

(cffi:defcfun ("zhash_first" zhash-first) :pointer
  (zhash :pointer))

(cffi:defcfun ("zhash_next" zhash-next) :pointer
  (zhash :pointer))

(cffi:defcfun ("zhash_cursor" zhash-cursor) :pointer
  (zhash :pointer))

(cffi:defcfun ("zhash_destroy" %zhash-destroy) :void
  (zhash :pointer))

;;
;; zpoller functions currently private-only for now
;;
(cffi:defcfun ("zpoller_new" zpoller-new) :pointer
  (first :pointer)
  &rest)

(cffi:defcfun ("zpoller_destroy" %zpoller-destroy) :void
  (zpoller :pointer))

(defun zpoller-destroy (zpoller)
  (declare (type cffi:foreign-pointer zpoller))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-aref ptr :pointer) zpoller)
    (%zpoller-destroy ptr)))

(cffi:defcfun ("zpoller_wait" zpoller-wait) :pointer
  (zpoller :pointer)
  (timeout :int))

(cffi:defcfun ("zpoller_expired" zpoller-expired) :bool
  (zpoller :pointer))

(cffi:defcfun ("zpoller_terminated" zpoller-terminated) :bool
  (zpoller :pointer))

(cffi:defcfun ("zpoller_set_nonstop" zpoller-set-nonstop) :void
  (zpoller :pointer)
  (nonstop :bool))

(defun zhash_destroy (zhash)
  (declare (type cffi:foreign-pointer zhash))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-aref ptr :pointer) zhash)
    (%zhash-destroy ptr)))

(defun zhash-to-alist (zhash)
  (declare (type cffi:foreign-pointer zhash) (optimize speed))
  (labels
      ((zhash-alist (z)
         (if (cffi::null-pointer-p z)
             nil
             (acons (cffi:foreign-string-to-lisp (zhash-cursor zhash))
                    (cffi:foreign-string-to-lisp z)
                    (zhash-alist (zhash-next zhash))))))
    (zhash-alist (zhash-first zhash))))

(defmethod print-object ((obj event) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "PEER ~a" (name obj))))

(defmethod print-object ((obj enter-event) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "PEER ~a ADDR ~a" (name obj) (peer-addr obj))
    (when (headers obj) (format stream " HEADERS ~a" (headers obj)))))

(defmethod print-object ((obj other-event) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "PEER ~a TYPE ~a" (name obj) (event-type obj))))

(defmethod print-object ((obj group-event) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "PEER ~a GROUP ~a" (name obj) (group obj))))

(defmethod print-object ((obj whisper-event) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "PEER ~a MESSAGE ~a" (name obj) (event-msg obj))))

(defmethod print-object ((obj shout-event) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "PEER ~a GROUP ~a MESSAGE ~a" (name obj) (group obj) (event-msg obj))))

;; Currently accepts only the raw C zmsgs, not instances of zmsg Lisp class
(defun zyre-zmsg-to-condition (zmsg)
  (declare (type cffi:foreign-pointer zmsg) (optimize speed))
  (let*
      ((type (zmsg-popstr zmsg))
       (uuid (zmsg-popstr zmsg))
       (name (zmsg-popstr zmsg))
       (event
         (match type
           ("ENTER"
            (let ((headers-packed (zmsg-pop zmsg)) headers)
              (when (not (cffi:null-pointer-p headers-packed))
                (setf headers (zhash-to-alist (zhash-unpack headers-packed)))
                (zframe-destroy headers-packed))
              (make-condition 'enter-event :uuid uuid :name name
                                           :headers headers :peer-addr (zmsg-popstr zmsg))))
           ("EXIT"
            (make-condition 'exit-event :uuid uuid :name name))
           ("JOIN"
            (make-condition 'join-event :uuid uuid :name name
                                        :group (zmsg-popstr zmsg)))
           ("LEAVE"
            (make-condition 'leave-event :uuid uuid :name name
                                         :group (zmsg-popstr zmsg)))
           ("LEADER"
            (make-condition 'leader-event :uuid uuid :name name
                                          :group (zmsg-popstr zmsg)))
           ("WHISPER"
            (make-condition 'whisper-event :uuid uuid :name name
                                           :msg (zmsg-popstr zmsg)))
           ("SHOUT"
            (make-condition 'shout-event :uuid uuid :name name
                                         :group (zmsg-popstr zmsg) :msg (zmsg-popstr zmsg)))
           (otherwise
            (make-condition 'other-event :uuid uuid :name name
                                         :event-type type)))))
    (zmsg-destroy zmsg)
    event))

(defun whisper (origin x &optional y)
  "Sends a Zyre WHISPER message. Forms are (whisper event message) or (whisper event
peer-uuid-or-uuids message). First form sends a message to the origin event's peer-uuid, second form
takes a uuid string or list of uuid strings. Event can be any valid event returned from a
zyre-pipe."
  (when (and y (consp x)) (mapc (lambda (z) (whisper origin z y)) x))
  (etypecase origin
    (zyre-state (zyre-whispers (raw-zyre origin) x (if y y "")))
    (event (zyre-whispers (raw-zyre (event-zyre origin))
                          (if y x (uuid origin))
                          (if y y x)))))

(defun shout (origin x &optional y)
  "Sends a Zyre SHOUT message to a group(s). Forms are (shout event message) or (shout event group
message). First form sends message to all joined group(s), second form sends to a named group. Event
can be any valid event returned from zyre-pipe."
  (etypecase origin
    (zyre-state
     (if y
         (zyre-shouts (raw-zyre origin) x y)
         (mapc (lambda (z) (zyre-shouts (raw-zyre origin) z x)) (group origin))))
    (event (shout (event-zyre origin) x y))))

(defun join (origin group)
  "Joins a Zyre group or list of groups."
  (when (consp group) (mapc (lambda (x) (join origin x)) group))
  (etypecase origin
    (zyre-state
     (push group (group origin))
     (zyre-join (raw-zyre origin) group))
    (event (join (event-zyre origin) group))))

(defun leave (origin group)
  "Leaves a Zyre group or list of groups."
  (when (consp group) (mapc (lambda (x) (leave origin x)) group))
  (etypecase origin
    (zyre-state
     (setf (group origin) (delete-if (lambda (x) (equal x group)) (group origin)))
     (zyre-leave (raw-zyre origin) group))
    (event (leave (event-zyre origin) group))))

(defun stop (origin)
  "Stops Zyre node."
  (etypecase origin
    (zyre-state
     (when (not (stopped origin))
       (zyre-stop (raw-zyre origin))
       (setf (stopped origin) t)))
    (event (stop (event-zyre origin)))))

(define-condition zyre-idle () ())
(defun stop-zyre () (invoke-restart 'stop-zyre))
(defun poll-some-ms (ms) (invoke-restart 'poll-some-ms ms))

(defun zyre-pipe (&key (name (cffi:null-pointer)) headers group interface port verbose)
  "Initializes and starts up a Zyre node and returns an infinite pipe of Zyre events as conditions.
Takes :name as the short name of the node, otherwise defaults to the first few digits of the UUID.
The :headers arg is a list of (key . value) string pairs, i.e. an alist. The :group key can either
be a string or a list of strings representing the groups to join on startup. :interface is a system
specific string representing a specific network interface to bind libzyre to. :port specifies to use
a nonstandard UDP port number for beacons.

The first event returned is of type start-event. This or any other event can be used as the origin
arg for functions join, leave, whisper, shout, and stop. These functions are not thread safe and can
only be used safely from the thread that advances the zyre-pipe. While there are no new events, the
pipe may block (depending on the handler of the zyre-idle condition, described below), but you can
always count on the first start-event being returned immediately.

When there are no new events, a condition of type zyre-idle is signalled. Valid restarts are
'use-value, 'poll-some-ms, and 'stop. The default action is to block the running thread.
'poll-some-ms takes a argument in milliseconds to continue waiting for, at which point the condition
will be resignalled. 'stop initiates a graceful exit from the zyre network. 'use-value allows a
handler to insert a sentinel value into the zyre-pipe output."
  (let* ((z (zyre-new name))
         (zp (zpoller-new (%zyre-socket z) :pointer (cffi:null-pointer)))
         (zyre (make-instance 'zyre-state :raw-zyre z :raw-zpoller zp
                                          :uuid (zyre-uuid z) :name (zyre-name z))))
    (declare (type cffi:foreign-pointer z zp) (type zyre-state zyre))
    (when verbose (zyre-set-verbose z))
    (zpoller-set-nonstop zp t)
    (tg:finalize zyre (lambda () (zpoller-destroy zp) (zyre-destroy z)))
    (when interface (zyre-set-interface z interface))
    (when group (join zyre group))
    (when (consp headers)
      (if (consp (car headers))
          (mapc (lambda (x) (zyre-set-header z (car x) (cdr x))) headers)
          (zyre-set-header z (car headers) (cdr headers))))
    (when port (zyre-set-port z port))
    (zyre-start z)
    (labels
        ((next-event-pipe ()
           (if (stopped zyre)
               :empty-pipe
               (pipe-cons (zyre-recv-maybe 0) (next-event-pipe))))
         (start-event () (make-condition 'start-event :uuid (uuid zyre) :name (name zyre)))
         (stop-event ()
           (prog1 (make-condition 'stop-event :uuid (uuid zyre) :name (name zyre))
             (stop zyre)))
         (%zyre-recv-maybe (to)
           (declare (type fixnum to) (optimize speed))
           (if (cffi::null-pointer-p (zpoller-wait zp to))
               (progn (signal 'zyre-idle) (%zyre-recv-maybe -1))
               (let ((x (%zyre-recv z)))
                 (if (cffi:null-pointer-p x)
                     (%zyre-recv-maybe 0)
                     (zyre-zmsg-to-condition x)))))
         (zyre-recv-maybe (timeout)
           (declare (type fixnum timeout) (optimize speed))
           (let ((to timeout))
             (loop
               (restart-case (return-from zyre-recv-maybe (%zyre-recv-maybe to))
                 (stop-zyre () (return-from zyre-recv-maybe (stop-event)))
                 (use-value (x) (return-from zyre-recv-maybe x))
                 (poll-some-ms (ms) (setf to ms))))))
         (annotate-event (ev)
           (when (typep ev 'event) (setf (event-zyre ev) zyre))
           (match ev
             ((enter-event (uuid u))
              (setf (gethash u (enter-events-by-uuid zyre)) ev))
             ((exit-event (uuid u))
              (remhash u (enter-events-by-uuid zyre))))
           ev))
      (pipe-transform #'annotate-event
                      (pipe-cons (start-event)
                                 (next-event-pipe))))))

(cffi:defcfun ("zsock_destroy" %zsock-destroy) :void
  (self :pointer))

(defun zsock-destroy (zsock)
  (declare (type (or zsock cffi:foreign-pointer) zsock))
  (etypecase zsock
    (zsock (zsock-destroy (raw-zsock zsock)))
    (cffi:foreign-pointer
     (cffi:with-foreign-object (ptr :pointer)
       (setf (cffi:mem-aref ptr :pointer) zsock)
       (%zsock-destroy ptr)))))

(cffi:defcfun ("zsock_new_pub" %zsock-new-pub) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_sub" %zsock-new-sub) :pointer
  (endpoint :string)
  (subscribe :string))
(cffi:defcfun ("zsock_new_req" %zsock-new-req) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_rep" %zsock-new-rep) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_dealer" %zsock-new-dealer) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_router" %zsock-new-router) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_push" %zsock-new-push) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_pull" %zsock-new-pull) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_xpub" %zsock-new-xpub) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_xsub" %zsock-new-xsub) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_pair" %zsock-new-pair) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_new_stream" %zsock-new-stream) :pointer
  (endpoint :string))
(cffi:defcfun ("zsock_endpoint" %zsock-endpoint) :string
  (zsock :pointer))

(defun zsock-endpoint (zs)
  (declare (type zsock zs))
  (let ((x (%zsock-endpoint (raw-zsock zs))))
    (values x (parse-integer x :start (1+ (position #\: x :from-end t)) :junk-allowed t))))

(defmethod print-object ((obj zsock) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "ENDPOINT ~a" (zsock-endpoint obj))))

(cffi:defcfun ("zframe_size" %zframe-size) size-t
  (zframe :pointer))

(cffi:defcfun ("zsock_set_sndbuf" zsock-set-sndbuf) :void
  (zsock :pointer)
  (sndbuf :int))

(cffi:defcfun ("zsock_set_sndhwm" zsock-set-sndhwm) :void
  (zsock :pointer)
  (sndhwm :int))

(cffi:defcfun ("zsock_set_bindtodevice" zsock-set-bindtodevice) :void
  (zsock :pointer)
  (interface :string))

(defun make-zsock (endpoint type &key (subscribe "") sndbuf sndhwm interface)
  "Creates a CZMQ zsock socket. See the czmq zsock(3) man page for more details on the meaning of
the various types of zsocks and endpoints."
  (let* ((zs (ematch type
               (:pub (%zsock-new-pub endpoint))
               (:sub (%zsock-new-sub endpoint subscribe))
               (:req (%zsock-new-req endpoint))
               (:rep (%zsock-new-rep endpoint))
               (:dealer (%zsock-new-dealer endpoint))
               (:router (%zsock-new-router endpoint))
               (:push (%zsock-new-push endpoint))
               (:pull (%zsock-new-pull endpoint))
               (:xpub (%zsock-new-xpub endpoint))
               (:xsub (%zsock-new-xsub endpoint))
               (:pair (%zsock-new-pair endpoint))
               (:stream (%zsock-new-stream endpoint))))
         (zsock (make-instance 'zsock :raw-zsock zs :zsock-type type)))
    (when (cffi:null-pointer-p zs) (error "zsock_new"))
    (when sndbuf (zsock-set-sndbuf zs sndbuf))
    (when sndhwm (zsock-set-sndhwm zs sndhwm))
    (when interface (zsock-set-bindtodevice zs interface))
    (tg:finalize zsock (lambda () (zsock-destroy zs)))
    zsock))

(cffi:defcfun ("zmsg_new" zmsg-new) :pointer)

(cffi:defcfun ("zmsg_send" zmsg-send) :int
  (zmsg :pointer)
  (sock :pointer))

(cffi:defcfun ("zframe_new" %zframe-new) :pointer
  (data :pointer)
  (size size-t))

(cffi:defcfun ("zframe_recv" %zframe-recv) :pointer
  (source :pointer))

(cffi:defcfun ("zframe_send" %%zframe-send) :int
  (zframe :pointer)
  (dest :pointer)
  (flags :int))

(cffi:defcfun ("zframe_data" %zframe-data) :pointer
  (zframe :pointer))

(defun %zframe-send (zframe dest flags)
  "After being sent, CZMQ destroys the frame."
  (declare (type cffi:foreign-pointer zframe dest)
           (type (unsigned-byte 32) flags)
           (optimize speed))
  (cffi:with-foreign-object (ptr :pointer)
    (setf (cffi:mem-aref ptr :pointer) zframe)
    (%%zframe-send ptr dest flags)))

(defun make-zframe (&key data size)
  "A zframe can be made by copying data from a lisp byte vector, or (if data is unspecified) a raw
foreign buffer that can be copied into directly by accessing the foreign-pointer via
zframe-foreign-data."
  (let* ((raw (if (and data size)
                  (cffi:with-pointer-to-vector-data (ptr data) (%zframe-new ptr size))
                  (%zframe-new (cffi:null-pointer) size)))
         (zf (make-instance 'zframe :raw-zframe raw)))
    (tg:finalize zf (lambda () (zframe-destroy raw)))
    zf))

(defun zframe-foreign-data (zf)
  "Returns the cffi:foreign-pointer for the czmq allocated and owned zframe."
  (declare (type zframe zf))
  (%zframe-data (raw-zframe zf)))

(defgeneric send (x y))

(defmethod send ((zs zsock) (zf zframe))
  "Sends a single zframe to a zsock.  The zframe is destroyed after this call."
  (let ((ret (%zframe-send (raw-zframe zf) (raw-zsock zs) 0)))
    (tg:cancel-finalization zf)
    (unless (zerop ret) (zframe-destroy (raw-zframe zf))) ;Destroy manually on error
    ret))

(cffi:defcfun ("zsock_send" zsock-send) :int
  (self :pointer)
  (picture :string)
  &rest)

(defmethod send ((zs zsock) (s string))
  "Sends a string to a zsock."
  (zsock-send (raw-zsock zs) "s" :string s))

(defun zframe-recv (zs)
  "This receives a raw zframe from a zsock.  GC finalization will handle destroying."
  (declare (type zsock zs) (optimize speed))
  (labels
      ((zframe-recv (raw-zsock)
         (let ((zf (%zframe-recv raw-zsock)))
           (if (cffi:null-pointer-p zf)
               (zframe-recv raw-zsock)
               zf))))
    (let* ((rawzf (zframe-recv (raw-zsock zs)))
           (zf (make-instance 'zframe :raw-zframe rawzf)))
      (tg:finalize zf (lambda () (zframe-destroy rawzf)))
      zf)))

(defun zmsg-recv (zs)
  "This receives a raw zmsg from a zsock.  GC finalization will handle destroying."
  (declare (type zsock zs) (optimize speed))
  (labels
      ((zmsg-recv (raw-zsock)
         (let ((zm (%zmsg-recv raw-zsock)))
           (if (cffi:null-pointer-p zm)
               (zmsg-recv raw-zsock)
               zm))))
    (let* ((rawzm (zmsg-recv (raw-zsock zs)))
           (zm (make-instance 'zmsg :raw-zmsg rawzm)))
      (tg:finalize zm (lambda () (zmsg-destroy rawzm)))
      zm)))

(cffi:defcfun ("zmsg_size" %zmsg-size) size-t
  (zmsg :pointer))

(defun zmsg-size (zm)
  (declare (type zmsg zm))
  "This returns the number of frames in the zmsg."
  (%zmsg-size (raw-zmsg zm)))

(cffi:defcfun ("zstr_recv" %zstr-recv) :pointer
  (source :pointer))

(defun zstr-recv (zs)
  "Retreives a string from the zsock. If the remote side did not send a string, results may be
undefined."
  (declare (type zsock zs) (optimize speed))
  (labels
      ((zstr-recv (raw-zsock)
         (let ((zstr (%zstr-recv raw-zsock)))
           (if (cffi:null-pointer-p zstr)
               (zstr-recv raw-zsock)
               zstr))))
    (let ((x (zstr-recv (raw-zsock zs))))
      (prog1 (cffi:foreign-string-to-lisp x)
        (zstr-free x)))))

(defvar *recv-type* 'string "This determines what the generic recv function receives frames as")

(defun recv (zs &optional (typ *recv-type*))
  "Receives a message from a zsock. The *recv-type* dynamic variable determines in what form the
messages are received as. Default is strings. This function will block if no messages are pending."
  (declare (type zsock zs) (optimize speed))
  (ematch typ
    ('zmsg (zmsg-recv zs))
    ('zframe (zframe-recv zs))
    ('string (zstr-recv zs))))

(defun recv-pipe (zs)
  "Represents the infinite pipe of received zeromq messages. The *recv-type* dynamic variable
determines in what form the messages are received as. Default is strings."
  (pipe-cons (recv zs) (recv-pipe zs)))

(defun blank-zframes-pipe (size)
  "Infinite pipe of zframe messages.  Intended for use in speed testing."
  (declare (optimize speed))
  (pipe-cons (make-zframe :size size) (blank-zframes-pipe size)))

(defun pipe-zsock-send (zsock pipe)
  "Sends via zeromq the messages in the pipe.  If pipe is infinite, this never returns."
  (declare (optimize speed))
  (unless (pipe-endp pipe)
    (send zsock (pipe-first pipe))
    (pipe-zsock-send zsock (pipe-rest pipe))))
