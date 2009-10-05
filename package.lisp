;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TECH; Base: 10 -*-
;;;

(defpackage tech
  (:use :common-lisp :alexandria :pergamum :iterate :bordeaux-threads :cl-opengl)
  (:shadowing-import-from :cl-opengl :rotate :scale :index)
  (:shadowing-import-from :cl-opengl-bindings :end :finish)
  (:export
  ))
