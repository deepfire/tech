;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: GENERIC; Base: 10 -*-
;;;
;;;  (c) copyright 2009 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :generic)


;;;
;;; Threading
;;;
(defclass auto-locked ()
  ((lock :initarg :lock))
  (:default-initargs :lock (make-recursive-lock)))

(defgeneric auto-lock (locked)
  (:method ((o auto-locked))
    (bordeaux-threads:acquire-lock (slot-value o 'lock))))

(defgeneric auto-unlock (locked)
  (:method ((o auto-locked))
    (bordeaux-threads:release-lock (slot-value o 'lock))))

(defmacro with-auto-lock (o &body body)
  `(with-lock-held ((slot-value ,o 'lock))
     ,@body))

(defmacro with-lock ((o lock-name) &body body)
  `(with-lock-held ((slot-value ,o ',(format-symbol t "~A-LOCK" lock-name)))
     ,@body))

(defmacro compare-and-swap (place old new)
  "Atomically stores NEW in PLACE if OLD matches the current value of PLACE.

Return value of T means that the operation was carried out.

PLACE must be an accessor form whose CAR is one of the following:

 CAR, CDR, FIRST, REST, SYMBOL-PLIST, SYMBOL-VALUE, SVREF

or the name of a DEFSTRUCT created accessor for a slot whose declared type is
either FIXNUM or T. Results are unspecified if the slot has a declared type
other then FIXNUM or T."
  #+sbcl `(eq (sb-ext:compare-and-swap ,place ,old ,new) ,old)
  #-(or sbcl) (error "~@<COMPARE-AND-SWAP not implemented.~:@>"))

(defmacro defcassable (name slot)
  `(defstruct ,name
     ,slot))

;;;
;;; Pool
;;;
(defclass pool (auto-locked)
  ((items :initarg :items :initform nil)))

(defmacro do-pool-items ((i pool) &body body)
  `(iter (for ,i in ,pool)
         ,@body))

(defgeneric pool-add (pool item) (:method ((o pool) item) (with-auto-lock o (push item (slot-value o 'items)))))
(defgeneric pool-clear (pool)    (:method ((o pool))      (with-auto-lock o (setf (slot-value o 'items) nil))))
(defgeneric pool-pop (pool)      (:method ((o pool))      (with-auto-lock o (pop (slot-value o 'items)))))

;;;
;;; Logging
;;;
(defclass log-manager () ())

(defvar *log-manager*)
(defvar *log-manager-trivial-stream* *standard-output*)

(defgeneric logmesg (level format-control &rest format-arguments)
  (:method ((level (eql :trivial)) format-control &rest format-arguments)
    (apply #'format *log-manager-trivial-stream* format-control format-arguments)))

;;;
;;; Generic
;;;
(defclass named ()
  ((name :accessor name :initarg :name)))

(defgeneric free (object))

(defun broken (reason) (error "~@<Breakage: ~A.~:@>" reason))
(defun origtodo (where) (warn "~@<Originally TODO in ~A.~:@>" where))
