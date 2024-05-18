
(in-package #:ftpush)

(defun basename (path)
  (namestring (make-pathname :name (pathname-name path)
                             :type (pathname-type path))))

(defun immediate-parent-dir (path)
  (make-pathname :directory (first (last (pathname-directory path)))))
