
(in-package #:ftpush)

(defvar *dry-run-p* nil)
(defvar *state-tracker* nil)
(defvar *local-dir* nil)
(defvar *remote-dir* nil)
(defvar *ftp-connection* nil)
(defvar *excludes* nil)
