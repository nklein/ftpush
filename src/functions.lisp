
(in-package #:ftpush)

(defun assert-empty-state ()
  (unless (and (null *state-tracker*)
               (null *local-dir*)
               (null *remote-dir*)
               (null *ftp-connection*)
               (null *excludes*))
    (error "Cannot invoke FTPUSH recursively, use FTPUSH-TREE and FTPUSH-FILE instead.")))

(defun assert-nonempty-state ()
  (when (or (null *state-tracker*)
            (null *local-dir*)
            (null *remote-dir*)
            (null *excludes*))
    (error "Can only invoke from inside FTPUSH")))

(defun assert-path-is-a-directory-that-exists (path)
  (assert (and (string= (namestring path)
                        (namestring (make-pathname :directory (pathname-directory path))))
               (probe-file path))))

(defmacro assert-non-null (&rest vars)
  (flet ((assert-one-non-null (var)
           `(assert ,var (,var) ,(format nil "Must specify ~A" var))))
    `(progn
       ,@(mapcar #'assert-one-non-null vars))))

(defun excludedp (local-path)
  (flet ((excludes-path (fn)
           (funcall fn local-path)))
    (find-if #'excludes-path *excludes*)))

(defun ftpush* (thunk &key
                        hostname
                        (username "ftp")
                        (password "none")
                        state-file
                        local-dir
                        remote-dir
                        excludes
                        dry-run-p)
  (assert-non-null hostname username password state-file local-dir remote-dir)
  (assert-path-is-a-directory-that-exists local-dir)

  (let ((*state-tracker* (make-instance 'state-tracker :state-file state-file))
        (*local-dir* (merge-pathnames (uiop:native-namestring local-dir)
                                      (uiop/os:getcwd)))
        (*remote-dir* remote-dir)
        (*excludes* excludes)
        (*dry-run-p* dry-run-p))
    (flet ((run-it ()
             (make-dirs *remote-dir*)
             (unwind-protect
                  (funcall thunk)
               (state-tracker-completed *state-tracker* *ftp-connection*))))
      (cond
        (*dry-run-p*
         (let ((*ftp-connection* nil))
           (break "NO FTP")
           (run-it)))
        (t
         (ftp:with-ftp-connection (*ftp-connection* :hostname hostname
                                                    :username username
                                                    :password password
                                                    :passive-ftp-p t)
           (unless *ftp-connection*
             (break "FAILED FTP"))
           (run-it)))))))

(defun ftpush-file (&key
                      local-file
                      remote-file)
  (assert-nonempty-state)
  (assert-non-null local-file remote-file)
  (let ((local-path (merge-pathnames local-file *local-dir*))
        (remote-path (merge-pathnames remote-file *remote-dir*)))
    (make-dirs (enough-namestring (directory-of remote-path) *remote-dir*)
               *remote-dir*)
    (unless (excludedp local-path)
      (push-file local-path remote-path))))

(defun ftpush-tree (&key
                      local-dir
                      remote-dir
                      excludes)
  (assert-nonempty-state)
  (let ((starting-remote-dir *remote-dir*)
        (made nil)
        (*excludes* (append *excludes* excludes))
        (*local-dir* (append-dirs *local-dir* local-dir))
        (*remote-dir* (append-dirs *remote-dir* remote-dir)))
    (unless (excludedp *local-dir*)
      (assert-path-is-a-directory-that-exists *local-dir*)
      (dolist (local-path (uiop:directory-files *local-dir*))
        (let* ((base (basename local-path))
               (remote-path (if base
                                (merge-pathnames base *remote-dir*)
                                *remote-dir*)))
          (unless (excludedp local-path)
            (unless made
              (prog1
                  (make-dirs remote-dir starting-remote-dir)
                (setf made t)))
            (push-file local-path remote-path))))
      (dolist (subdir (uiop:subdirectories *local-dir*))
        (let ((just-dir (enough-namestring subdir *local-dir*)))
          (ftpush-tree :local-dir just-dir
                       :remote-dir just-dir))))))
