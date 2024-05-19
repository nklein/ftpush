
(defpackage #:ftpush
  (:use #:cl)
  (:export :ftpush-remote
           :remote-open-connection
           :remote-close-connection
           :remote-make-directory
           :remote-store-file
           :remote-delete-file)
  (:export :ftpush
           :ftpush-file
           :ftpush-tree)
  (:export :file-matcher
           :extension-matcher
           :path-matcher
           :directory-matcher
           :read-password-from-file))
