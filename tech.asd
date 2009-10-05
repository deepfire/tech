;;; -*- Mode: Lisp -*-

(defpackage :tech.system
  (:use :cl :asdf))

(in-package :tech.system)

(defsystem :tech
  :depends-on (:alexandria :pergamum :cffi)
  :components
  ((:file "package")
   (:file "axis-aligned-box" :depends-on ("package"))
   (:file "tech" :depends-on ("package"))))
