
(in-package #:ftpush)

(defmacro ftpush ((&rest args
                   &key
                     hostname
                     username
                     password
                     state-file
                     local-dir
                     remote-dir
                     excludes
                     dry-run-p)
                  &body
                    body)
  (declare (ignore hostname username password state-file local-dir remote-dir excludes dry-run-p))
  `(ftpush* (lambda () ,@body) ,@args))
