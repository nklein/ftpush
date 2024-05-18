
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

(defmacro ftpush-file ((&rest
                          args
                        &key
                          local-file
                          remote-file))
  (declare (ignore local-file remote-file))
  `(ftpush-file* ,@args))

(defmacro ftpush-tree ((&rest
                          args
                        &key
                          local-dir
                          remote-dir
                          excludes))
  (declare (ignore local-dir remote-dir excludes))
  `(ftpush-tree* ,@args))
