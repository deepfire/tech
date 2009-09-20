;;; -*- Mode: Lisp -*-

(defpackage :tech.system
  (:use :cl :asdf))

(in-package :tech.system)

(defsystem :tech
  :depends-on (:alexandria :pergamum)
  :components
  ((:file "package")
   (:file "tech" :depends-on ("package"))))
