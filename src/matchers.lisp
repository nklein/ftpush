
(in-package #:ftpush)

(defmacro make-matcher ((regex-var path-var) &body form)
  (let ((scanner-var (gensym "SCANNER-"))
        (part-var (gensym "PART-")))
    `(let ((,scanner-var (cl-ppcre:create-scanner ,regex-var)))
       (lambda (,path-var)
         (let ((,part-var ,(first form)))
           (and ,part-var
                (funcall ,scanner-var ,part-var 0 (length ,part-var))
                t))))))

(defun file-matcher (regex)
  (make-matcher (regex path)
    (basename path)))

(defun path-matcher (regex)
  (make-matcher (regex path)
    (namestring path)))

(defun extension-matcher (regex)
  (make-matcher (regex path)
    (pathname-type path)))

(defun directory-matcher (regex)
  (make-matcher (regex path)
    (namestring (make-pathname :directory (pathname-directory path)))))
