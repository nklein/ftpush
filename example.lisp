(ql:quickload :ftpush)

(use-package :ftpush)

(defun read-password-from-file (file)
  "todo")

(ftpush (:hostname "ftp.webhost.com"
         :username "mylogin"
         :password (read-password-from-file)
         :state-file #P"~/.ftpush/webapp.ftpush"
         :local-dir #P"~/src/webapp/"
         :remote-dir #P"/webapp/"
         :excludes (list (file-matcher "\\A.gitignore\\z")
                         (path-matcher "/secrets/.*\\.local\\z")
                         (extension-matcher "\\Asample\\z")
                         (directory-matcher "/.git/\\z")
                         (lambda (local-filepath)
                           (declare (ignore local-filepath))
                           ;; do something with local-filepath
                           nil))
         :dry-run-p t)

  (ftpush-file (:local-file #P"config/nginx/default.conf"
                :remote-file #P"config/nginx/conf.d/default.conf"))

  (ftpush-tree (:local-dir #P"secrets/"
                :remote-dir #P"secrets/"))

  (ftpush-tree (:local-dir #P"client/html/"
                :remote-dir #P"html/"
                :excludes (list (file-matcher "\\A.keep\\z"))))

  (ftpush-tree (:local-dir #P"client/src/dist/"
                :remote-dir #P"html/js/"))

  (ftpush-tree (:local-dir #P"server/src/html/api/"
                :remote-dir #P"html/api/"))

  (ftpush-tree (:local-dir #P"server/src/private/"
                :remote-dir #P"private/"))

  (ftpush-tree (:local-dir #P"server/src/vendor/"
                :remote-dir #P"vendor/")))
