
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

(defmacro require-non-null (&rest vars)
  (flet ((assert-non-null (var)
           `(assert ,var (,var) ,(format nil "Must specify ~A" var))))
    `(progn
       ,@(mapcar #'assert-non-null vars))))

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
  (require-non-null hostname username password state-file local-dir remote-dir)
  (assert (and (string= (namestring local-dir)
                        (namestring (make-pathname :directory (pathname-directory local-dir))))
               (probe-file local-dir)))

  (let ((*state-tracker* (make-instance 'state-tracker :state-file state-file))
        (*local-dir* (uiop:native-namestring local-dir))
        (*remote-dir* remote-dir)
        (*excludes* excludes)
        (*dry-run-p* dry-run-p))
    (flet ((run-it ()
             (unwind-protect
                  (funcall thunk)
               (state-tracker-completed *state-tracker* *ftp-connection*))))
      (cond
        (*dry-run-p*
         (let ((*ftp-connection* nil))
           (run-it)))
        (t
         (ftp:with-ftp-connection (*ftp-connection* :hostname hostname
                                                    :username username
                                                    :password password
                                                    :passive-ftp-p t)
           (run-it)))))))

(defun ftpush-file* (&key
                       local-file
                       remote-file)
  (assert-nonempty-state)
  (assert (or local-file remote-file))
  (let ((local-file (or local-file remote-file))
        (remote-file (or remote-file local-file)))
    (let ((local-path (merge-pathnames local-file *local-dir*))
          (remote-path (merge-pathnames remote-file *remote-dir*)))
      (unless (excludedp local-path)
        (push-file local-path remote-path)))))

(defun append-dirs (a b)
  (if (null a)
      b
      (merge-pathnames a b)))

(defun ftpush-tree* (&key
                       local-dir
                       remote-dir
                       excludes)
  (assert-nonempty-state)
  (let ((*excludes* (append *excludes* excludes))
        (*local-dir* (append-dirs local-dir *local-dir*))
        (*remote-dir* (append-dirs remote-dir *remote-dir*)))
    (dolist (local-path (uiop:directory-files *local-dir*))
      (let* ((base (basename local-path))
             (remote-path (merge-pathnames base *remote-dir*)))
        (unless (excludedp local-path)
          (push-file local-path remote-path))))
    (dolist (subdir (uiop:subdirectories *local-dir*))
      (let ((just-dir (enough-namestring subdir *local-dir*)))
        (ftpush-tree* :local-dir just-dir
                      :remote-dir just-dir)))))
