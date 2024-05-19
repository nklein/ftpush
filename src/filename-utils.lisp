
(in-package #:ftpush)

(defun basename (path)
  (namestring (make-pathname :name (pathname-name path)
                             :type (pathname-type path))))

(defun directory-of (path)
  (make-pathname :directory (pathname-directory path)))

(directory-of #P"/foo/bar/baz/goo.png")
(directory-of #P"/foo/bar/baz/")
