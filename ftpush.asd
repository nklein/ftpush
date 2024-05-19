;;;; ftpush.asd

(asdf:defsystem #:ftpush
  :description "Domain-specific language for pushing files to ftp sites."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20240518"
  :depends-on (#:uiop #:cl-ppcre #:md5 #:cl-base64)
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src"
    :components ((:file "package")
                 (:file "remote-interface" :depends-on ("package"))
                 (:file "specials" :depends-on ("package"))
                 (:file "filename-utils" :depends-on ("package"))
                 (:file "ftp-helpers" :depends-on ("package"
                                                   "specials"
                                                   "filename-utils"))
                 (:file "matchers" :depends-on ("package"
                                                "specials"
                                                "filename-utils"))
                 (:file "state-tracker" :depends-on ("package"))
                 (:file "functions" :depends-on ("package"
                                                 "specials"
                                                 "ftp-helpers"
                                                 "state-tracker"))))))

(asdf:defsystem #:ftpush/ftp
  :description "Remote interface to use CL-FTP as an FTPUSH-REMOTE for FTPUSH."
  :author "Patrick Stein <pat@nklein.com>"
  :license "UNLICENSE"
  :version "0.1.20240518"
  :depends-on (#:ftpush #:cl-ftp)
  :components
  ((:static-file "README.md")
   (:static-file "UNLICENSE.txt")
   (:module "src/providers/ftp"
    :components ((:file "package")
                 (:file "remote" :depends-on ("package"))))))
