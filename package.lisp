;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TECH; Base: 10 -*-
;;;

(defpackage generic
  (:use :common-lisp :alexandria :pergamum :iterate :bordeaux-threads)
  (:export
   #:auto-locked
   #:auto-lock
   #:auto-unlock
   #:with-auto-lock
   #:with-lock
   #:compare-and-swap
   #:defcassable

   #:pool
   #:do-pool-items
   #:pool-add
   #:pool-clear
   #:pool-pop

   #:logmesg
   #:*log-manager-trivial-stream*

   #:named
   #:name

   #:free

   #:broken
   #:origtodo))

(defpackage archive
  (:use :common-lisp :alexandria :pergamum :iterate
        :generic)
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
  (:use :common-lisp :alexandria :pergamum :iterate :cl-opengl
        :generic :archive)
  (:shadowing-import-from :cl-opengl :rotate :scale :index)
  (:shadowing-import-from :cl-opengl-bindings :end :finish)
  (:export
  ))
