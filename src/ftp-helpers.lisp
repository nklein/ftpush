
(in-package #:ftpush)


(defun read-password-from-file (file)
  (with-open-file (in file)
    (read-line in)))

(defun hash-file (path)
  (subseq (cl-base64:usb8-array-to-base64-string (md5:md5sum-file path))
          0 22))

(defun append-dirs (a b)
  (cond
    ((null a)
     b)
    ((null b)
     a)
    (t
     (merge-pathnames b a))))

(defun make-dir (path)
  (unless (state-tracker-previously-made-directory-p *state-tracker* path)
    (remote-make-directory *remote* (namestring path))
    (state-tracker-track-directory *state-tracker* path)))

(defun make-dirs (path &optional done)
  (labels ((rec (done todo absolute-or-relative)
             (cond
               (todo
                (destructuring-bind (cur &rest todo) todo
                  (let* ((cur (make-pathname :directory (list absolute-or-relative cur)))
                         (done (append-dirs done cur)))
                    (make-dir done)
                    (rec done todo :relative))))
               (t done))))
    (destructuring-bind (absolute-or-relative &rest todo) (pathname-directory path)
      (rec done todo absolute-or-relative))))

(defun push-file (local-path remote-path)
  (let ((file-hash (hash-file local-path)))
    (cond
      ((state-tracker-previously-uploaded-p *state-tracker* remote-path file-hash)
       (format *debug-io* "UP-TO-DATE: ~A~%" remote-path))

      (t
       (remote-store-file *remote* (namestring local-path) (namestring remote-path))))
    (state-tracker-track-file *state-tracker* remote-path file-hash)))

(defun remove-file (remote-path)
  (remote-delete-file *remote* (namestring remote-path)))
