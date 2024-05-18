
(in-package #:ftpush)

(defclass state-tracker ()
  ((state-file :initarg :state-file :reader state-tracker-state-file)
   (state :accessor state-tracker-state :initform nil)
   (start-state :accessor state-tracker-start-state)))

(defun read-state-file (filename)
  (declare (ignore filename))
  (values))

(defun write-state-file (filename state)
  (declare (ignore filename state))
  (values))

(defun remove-remote-files-not-used-this-time (tracker)
  (declare (ignore tracker))
  ;; see what is left in start-state that isn't in state and remove those remotely
  (values))

(defmethod initialize-instance :after ((instance state-tracker) &key state-file &allow-other-keys)
  (setf (state-tracker-start-state instance) (read-state-file state-file)))

(defgeneric state-tracker-completed (tracker ftp)
  (:method ((tracker state-tracker) ftp)
    (remove-remote-files-not-used-this-time tracker)
    (write-state-file (state-tracker-state-file tracker)
                      (state-tracker-state tracker))))

(defgeneric state-tracker-previously-uploaded-p (tracker remote-path file-hash)
  (:method ((tracker state-tracker) remote-path file-hash)
    (declare (ignore remote-path file-hash))
    ;; check for file in start-state or current-state and see if it has the same hash
    ;; return if it does exist and has that same hash
    nil))

(defgeneric state-tracker-track-file (tracker remote-file file-hash)
  (:method ((tracker state-tracker) remote-file file-hash)
    (declare (ignore remote-file file-hash))
    ;; add the remote file to the state with the given file-hash
    ;; maybe remove it from the start-state? if so, rename start-state to remaining?
    (values)))
