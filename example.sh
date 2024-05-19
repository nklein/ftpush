#!/usr/bin/env sbcl --script

(require :asdf)

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(ql:quickload '(:ftpush :ftpush/ftp :unix-opts) :silent t)

(opts:define-opts
  (:name :dry-run
   :description "do not actually upload any files"
   :short #\n
   :long "dry-run")
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help"))

(defun push-web-app (&optional remote)
  (ftpush:ftpush (:remote-provider remote
                  :state-file #P"~/.ftpush/webapp.ftpush"
                  :local-dir #P"example-webapp/"
                  :remote-dir #P"/webapp/"
                  :excludes (list (ftpush:file-matcher "\\A.gitignore\\z")
                                  (ftpush:extension-matcher "\\Ajson\\z")
                                  (ftpush:directory-matcher "/private/\\z")
                                  (ftpush:directory-matcher "/node_modules/\\z")
                                  (lambda (local-filepath)
                                    (declare (ignore local-filepath))
                                    ;; do something with local-filepath
                                    nil)))

    (ftpush:ftpush-tree :excludes (list (ftpush:path-matcher "/html/.*\\orig\\z")
                                        (ftpush:file-matcher "\\AREADME.md\\z")))

    (ftpush:ftpush-file :local-file #P"node_modules/bootstrap/dist/css/bootstrap.min.css"
                        :remote-file #P"/webapp/html/css/bootstrap.min.css")

    (ftpush:ftpush-file :local-file #P"node_modules/bootstrap/dist/js/bootstrap.min.js"
                        :remote-file #P"/webapp/html/js/bootstrap.min.js")))

(let ((options (opts:get-opts)))
  (cond
    ((getf options :help)
     (opts:describe
      :prefix "Example program showing how to use the FTPUSH library."
      :usage-of "example.sh"))
    (t
     (push-web-app (unless (getf options :dry-run)
                     (make-instance 'ftpush/ftp:ftpush-remote-ftp
                                    :hostname "ftp.nklein.com"
                                    :username "nkleincom"
                                    :password (ftpush:read-password-from-file #P "~/.ftpush/nkleincom.passwd")))))))
