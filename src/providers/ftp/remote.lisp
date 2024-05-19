
(in-package #:ftpush/ftp)

(defclass ftpush-remote-ftp (ftpush:ftpush-remote)
  ((connection :initform nil :accessor ftpush-remote-ftp-connection)
   (hostname :initarg :hostname :reader ftpush-remote-ftp-hostname)
   (port :initarg :port :reader ftpush-remote-ftp-port)
   (username :initarg :username :reader ftpush-remote-ftp-username)
   (password :initarg :password :reader ftpush-remote-ftp-password)
   (passive-ftp-p :initarg :passive-ftp-p :reader ftpush-remote-ftp-passive-ftp-p))
  (:default-initargs
   :hostname (error "Must provide HOSTNAME")
   :port 21
   :username (error "Must provide USERNAME")
   :password (error "Must provide PASSWORD")
   :passive-ftp-p t))

(defun print-error (err)
  (let ((*print-escape* nil))
    (print-object err *debug-io*)))

(defmethod ftpush:remote-open-connection ((remote ftpush-remote-ftp))
  (setf (ftpush-remote-ftp-connection remote)
        (make-instance 'ftp:ftp-connection
                       :hostname (ftpush-remote-ftp-hostname remote)
                       :port (ftpush-remote-ftp-port remote)
                       :username (ftpush-remote-ftp-username remote)
                       :password (ftpush-remote-ftp-password remote)
                       :passive-ftp-p (ftpush-remote-ftp-passive-ftp-p remote))))

(defmethod ftpush:remote-close-connection ((remote ftpush-remote-ftp))
  (prog1
      (ftp:close-connection (ftpush-remote-ftp-connection remote))
    (setf (ftpush-remote-ftp-connection remote) nil)))

(defmethod ftpush:remote-make-directory ((remote ftpush-remote-ftp) (directory string))
  (handler-case
      (ftp:send-mkd-command (ftpush-remote-ftp-connection remote) directory)
    (ftp:ftp-error (err)
      (unless (search " exists" (ftp::error-message err))
        (error err)))))

(defmethod ftpush:remote-store-file ((remote ftpush-remote-ftp) (local-path string) (remote-path string))
  (ftp:store-file (ftpush-remote-ftp-connection remote) local-path remote-path))

(defmethod ftpush:remote-delete-file ((remote ftpush-remote-ftp) (remote-path string))
    (handler-case
        (ftp:send-dele-command (ftpush-remote-ftp-connection remote) remote-path)
    (ftp:ftp-error (err)
      (unless (search " such file or directory" (ftp::error-message err))
        (error err))
      (print-error err))))
