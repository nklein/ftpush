
(in-package #:ftpush)

(defun hash-file (path)
  (subseq (cl-base64:usb8-array-to-base64-string (md5:md5sum-file path))
          0 22))

(defun push-file (local-path remote-path)
  (let ((file-hash (hash-file local-path)))
    (cond
      ((state-tracker-previously-uploaded-p *state-tracker* remote-path file-hash)
       (format *debug-io* "~A: ~A => ~A [UP-TO-DATE]~%" file-hash local-path remote-path))

      (*dry-run-p*
       (format *debug-io* "~A: ~A => ~A [SIM]~%" file-hash local-path remote-path))

      (t
       (ftp:store-file *ftp-connection* (namestring local-path) (namestring remote-path))
       (format *debug-io* "~A: ~A => ~A~%" file-hash (namestring local-path) (namestring remote-path))))
    (state-tracker-track-file *state-tracker* remote-path file-hash)))
