;;; -*- Mode: Lisp -*-

(defpackage :tech.system
  (:use :cl :asdf))

(in-package :tech.system)

(defsystem :tech
  :depends-on (:alexandria :pergamum :cffi :bordeaux-threads :cl-opengl)
  :components
  ((:file "package")
   ;;
   (:file "axis-aligned-box" :depends-on ("package"))
   (:file "generic" :depends-on ("package"))
   ;;
   (:file "archive" :depends-on ("generic"))
   ;;
   (:file "resource" :depends-on ("archive"))
   (:file "tech" :depends-on ("archive"))))
