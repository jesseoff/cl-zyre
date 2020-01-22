#|
Copyright 2020 Jesse Off <jesseoff@me.com>

Distributed under the MIT license (see LICENSE file)
|#

(in-package #:zyre)

(defconstant +zyredir+ #p"/tmp/zyre/")

(defun zyredir ()
  "Maintains a directory of *.json files containing all currently online peers.
Removes files when peers exit. JSON data contains name, address, and headers, respectively."
  (labels
      ((json-f (u)
         (merge-pathnames (make-pathname :name u :type "json" :defaults +zyredir+) +zyredir+))
       (json-rm (&optional (l (directory (merge-pathnames
                                          (make-pathname :name :wild :type "json") +zyredir+))))
         (unless (endp l)
           (delete-file (car l))
           (json-rm (cdr l)))))
    (ensure-directories-exist +zyredir+)
    (json-rm)                           ;Clear directory of *.json
    (pipe-mapc
     (lambda (x)
       (match x
             ((enter-event (uuid u) (headers h) (name n) (peer-addr a))
              (with-open-file (s (json-f u) :direction :output :if-exists :supersede)
                (cl-json:encode-json (list n a h) s)))
             ((exit-event (uuid u))
              (let ((f (json-f u))) (when (probe-file f) (delete-file f))))))
         (zyre-pipe))))
