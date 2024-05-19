(ql:quickload :ftpush)

(use-package :ftpush)

(defun push-web-app (dry-run-p)
  (ftpush (:hostname "ftp.nklein.com"
           :username "nkleincom"
           :password (read-password-from-file #P "~/.ftpush/webapp.passwd")
           :state-file #P"~/.ftpush/webapp.ftpush"
           :local-dir #P"example-webapp/"
           :remote-dir #P"/webapp/"
           :excludes (list (file-matcher "\\A.gitignore\\z")
                           (extension-matcher "\\Ajson\\z")
                           (directory-matcher "/private/\\z")
                           (directory-matcher "/node_modules/\\z")
                           (lambda (local-filepath)
                             (declare (ignore local-filepath))
                             ;; do something with local-filepath
                             nil))
           :dry-run-p dry-run-p)

    (ftpush-tree :excludes (list (path-matcher "/html/.*\\orig\\z")))

    (ftpush-file :local-file #P"node_modules/bootstrap/dist/css/bootstrap.min.css"
                 :remote-file #P"/webapp/html/css/bootstrap.min.css")

    (ftpush-file :local-file #P"node_modules/bootstrap/dist/js/bootstrap.min.js"
                 :remote-file #P"/webapp/html/js/bootstrap.min.js")))

(push-web-app nil)
