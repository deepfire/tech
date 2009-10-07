;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TECH; Base: 10 -*-
;;;

(defpackage archive
  (:use :common-lisp :alexandria :pergamum :iterate :bordeaux-threads)
  (:export
   #:archive
   #:archive-name
   #:archive-read-only-p
   #:archive-case-sensitive-p

   #:archive-init
   #:archive-open
   #:archive-create
   #:archive-remove
   #:archive-exists-p
   #:archive-modified-time
   #:archive-list
   #:archive-list-file-info
   #:archive-find
   #:archive-find-file-info
   #:archive-load
   #:archive-unload
   
   #:archive-manager
   #:archive-manager-load
   #:*archive-manager*))

(defpackage tech
  (:use :common-lisp :alexandria :pergamum :iterate :bordeaux-threads :cl-opengl
        :archive)
  (:shadowing-import-from :cl-opengl :rotate :scale :index)
  (:shadowing-import-from :cl-opengl-bindings :end :finish)
  (:export
  ))
