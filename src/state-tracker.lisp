
(in-package #:ftpush)

(defun make-state ()
  (make-hash-table :test 'equal))

(defun add-state (state file hash)
  (setf (gethash file state) hash))

(defun get-state (state file)
  (values (gethash file state)))

(defun mapstate (fn state)
  (maphash fn state))

(defclass state-tracker ()
  ((state-file :initarg :state-file :reader state-tracker-state-file)
   (state :accessor state-tracker-state :initform (make-state))
   (start-state :accessor state-tracker-start-state)))

(defun read-state-file (filename)
  (let ((state (make-state)))
    (handler-case
        (with-open-file (in filename)
          (loop :for row := (read in nil)
                :while row
                :do (destructuring-bind (file hash) row
                      (add-state state file hash))))
      (file-error (ex)
        (declare (ignore ex))
        (format *debug-io* "Could not read state file. Assuming first run.~%")))
    (values state)))

(defun write-state-file (filename state)
  (with-open-file (out (uiop:ensure-pathname filename :ensure-directories-exist t)
                       :direction :output
                       :if-does-not-exist :create
                       :if-exists :supersede)
    (with-standard-io-syntax
      (flet ((print-state (file hash)
               (print (list file hash) out)))
        (mapstate #'print-state state)))))

(defun remove-remote-files-not-used-this-time (tracker)
  (let ((state (state-tracker-state tracker))
        (start-state (state-tracker-start-state tracker))
        (missing nil))
    (flet ((check (file hash)
             (declare (ignore hash))
             (unless (get-state state file)
               (pushnew file missing))))
      (mapstate #'check start-state))
    (dolist (file missing missing)
      (remove-file file))))

(defmethod initialize-instance :after ((instance state-tracker) &key state-file &allow-other-keys)
  (setf (state-tracker-start-state instance) (read-state-file state-file)))

(defgeneric state-tracker-completed (tracker ftp)
  (:method ((tracker state-tracker) ftp)
    (remove-remote-files-not-used-this-time tracker)
    (unless *dry-run-p*
      (write-state-file (state-tracker-state-file tracker)
                        (state-tracker-state tracker)))))

(defgeneric state-tracker-previously-uploaded-p (tracker remote-file file-hash)
  (:method ((tracker state-tracker) remote-file file-hash)
    (equal file-hash
           (or (get-state (state-tracker-state tracker) remote-file)
               (get-state (state-tracker-start-state tracker) remote-file)))))

(defgeneric state-tracker-track-file (tracker remote-file file-hash)
  (:method ((tracker state-tracker) remote-file file-hash)
    (add-state (state-tracker-state tracker) remote-file file-hash)
    ;; maybe remove it from the start-state? if so, rename start-state to remaining?
    (values)))
