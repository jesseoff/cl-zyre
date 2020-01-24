(in-package #:zyre)

(defun tock-client ()
  "Joins Zyre group named tick-tock and waits for shouts. Shouts contain a remote tock-server's send
timestamp, which is then compared to the local tock-client's reception timestamp and the offset is
printed. This is a measure of how well locked the two clocks are to a reference time and how long
and jittery the network stack + Zyre shouting is."
  (labels
      ((tick (msg peer)
         (let ((n (local-time:now)) (o (local-time:parse-timestring msg)))
           (format t ">> ~a msec (~a)~%"
                   (/ (ltd:duration-as (ltd:timestamp-difference n o) :nsec) 1000000.0)
                   peer)))
       (handler (x) (match x ((shout-event (event-msg m) (name n)) (tick m n)))))
    (pipe-mapc #'handler (zyre-pipe :group "tick-tock" :name "tock-client"))))

(defun ms-until (ts now)
  (let ((ns (ltd:duration-as (ltd:timestamp-difference ts now) :nsec)))
    (if (< ns 10000000) 0 (- (ceiling ns 1000000) 10))))

(defun tock-server ()
  "Joins Zyre group named tick-tock and shouts the system's current timestamp once per second, on
the second, as precisely as possible. Wakes up from zpolling 10ms early to busy-wait the rest of the
way to make sure. (This reduces jitter by a few milliseconds at the cost of some CPU)"
  (let* ((zp (zyre-pipe :group "tick-tock" :name (machine-instance)))
         (z (pipe-first zp))            ;z will be the Zyre start-event
         (to (local-time:timestamp-minimize-part
              (local-time:timestamp+ (local-time:now) 2 :sec)
              :nsec))
         now)
    (handler-bind ((zyre-idle
                     (lambda (x) (declare (ignore x))
                       (setf now (local-time:now))
                       (if (local-time:timestamp<= to now)
                           (use-value 'quiescent)
                           (poll-some-ms (ms-until to now))))))
      (loop
        (setf zp (pipe-sink-until (lambda (x) (eq x 'quiescent)) zp))
        (if (pipe-endp zp) (return-from tock-server nil))
        (format t ">> Tock! (~a)~%" (local-time:format-timestring nil now))
        (shout z "tick-tock" (local-time:format-timestring nil now))
        (setf to (local-time:timestamp+ to 1 :sec) zp (pipe-rest zp))))))
