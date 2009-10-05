;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: TECH; Base: 10 -*-
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

(in-package :tech)

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

;;;
;;; Resource
;;;
(defcassable resource-state value)

(defclass resource (named auto-locked)
  ((state :initarg :load-state :type resource-state)
   (statecount :reader resource-statecount)
   (creator :reader resource-creator :type (or null resource-manager) :initarg :creator)
   (groupname :reader resource-groupname :type symbol :initarg :groupname)
   (handle :accessor resource-handle :initarg :handle)
   (size :reader :resource-size :initarg :size)
   
   (background-load :reader resource-background-load-p :initarg :background-load)
   (origin :reader resource-origin :initarg :origin)
   (manual-load :reader resource-manual-load-p :initarg :manual-load)
   (manual-loader :reader resource-manual-loader :type manual-resource-loader :initarg :manual-loader)
   (listeners :reader resource-listeners :type list)
   (listener-lock ))
  (:default-initargs
   :state :unloaded :statecount 0 :manager nil :handle 0 :background-load nil :size 0 :manual-load nil :manual-loader nil)
  (:documentation
   "Abstract class representing a loadable resource (e.g. textures, sounds etc)
Resources are data objects that must be loaded and managed throughout
an application. A resource might be a mesh, a texture, or any other
piece of data - the key thing is that they must be identified by 
a name which is unique, must be loaded only once,
must be managed efficiently in terms of retrieval, and they may
also be unloadable to free memory up when they have not been used for
a while and the memory budget is under stress.

All Resource instances must be a member of a resource group; see
ResourceGroupManager for full details.

Subclasses must implement:
 - the loadImpl() and unloadImpl() methods - mSize must be set after loadImpl()
 - StringInterface ParamCommand and ParamDictionary setups in order to allow
setting of core parameters (prior to load) through a generic interface."))

(defgeneric resource-compute-size (resource))
(defgeneric resource-change-ownership (resource resource-group))

(defgeneric resource-do-prepare (resource))
(defgeneric resource-prepare (resource))
(defgeneric resource-fire-preparing-complete (resource))

(defgeneric resource-unprepare (resource))

(defgeneric resource-do-pre-load (resource))
(defgeneric resource-load (resource backgroundp))
(defgeneric resource-do-post-load (resource))
(defgeneric resource-fire-loading-complete (resource))

(defgeneric resource-unload (resource))
(defgeneric resource-fire-unloading-complete (resource))

(defgeneric resource-reload (resource))
(defgeneric resource-touch (resource))

(defclass manual-resource-loader ()
  ())

(defclass resource-loader ()
  ((resource-type :accessor resource-loader-resource-type :initarg :resource-type)))

(defclass manual-resource-loader (resource-loader)  ())

(defgeneric resource-loader-prepare-resource (loader resource))
(defgeneric resource-loader-load-resource (loader resource))

(defclass resource-manager ()
  ())

(defgeneric resource-manager-notify-resource-loaded (manager resource))
(defgeneric resource-manager-notify-resource-unloaded (manager resource))
(defgeneric resource-manager-notify-resource-touched (manager resource))

(defclass resource-group-manager () ())

(defvar *resource-group-manager*)

(defgeneric resource-group-manager-find-group-containing-resource (manager resource-name))
(defgeneric resource-group-manager-notify-resource-group-changed (manager old-group resource))

(defclass resource-listener () ())

(defgeneric resource-listener-background-prepare-complete (listener resource))
(defgeneric resource-listener-background-load-complete (listener resource))
(defgeneric resource-listener-prepare-complete (listener resource))
(defgeneric resource-listener-load-complete (listener resource))
(defgeneric resource-listener-unload-complete (listener resource))

(defun resource-try-swap-state (r old new)
  (compare-and-swap (resource-state-value (slot-value r 'state)) old new))

(defun resource-state (r)
  (resource-state-value (slot-value r 'state)))

(defun set-resource-state (r state)
  (setf (resource-state-value (slot-value r 'state)) state))

(defun resource-set-state-dirty (r)
  (incf (slot-value r 'statecount)))

(defun resource-escalate-loading (r)
  (resource-load r t))

(defmethod resource-change-ownership ((r resource) resource-group-name)
  (let ((old-group-name (resource-groupname r)))
    (unless (eq old-group-name resource-group-name)
      (setf (slot-value r 'groupname) resource-group-name)
      (resource-group-manager-notify-resource-group-changed *resource-group-manager* old-group-name r))))

(defmethod resource-prepare :around ((r resource))
  (let ((oldstate (resource-state r)))
    (when (or (eq oldstate :preparing) (eq oldstate :loading))
      (unless (resource-try-swap-state r :unloaded :preparing)
        (iter (while (eq (resource-state r) :preparing))
              (auto-lock r)
              (auto-unlock r))
        (return-from resource-prepare)))
    (handler-case (with-auto-lock r
                    (if (resource-manual-load-p r)
                        (if-let ((loader (resource-manual-loader r)))
                          (resource-loader-prepare-resource loader r)
                          (logmesg :trivial "WARNING: ~S instance '~S' was defined as manually loaded, but no manual loader was provided. This Resource will be lost if it has to be reloaded."
                                   (resource-loader-resource-type loader) (name r)))
                        (progn
                          (when (eq (resource-groupname r) :autodetect-resource-group)
                            (resource-change-ownership r (resource-group-manager-find-group-containing-resource *resource-group-manager* (name r))))
                          (call-next-method))))
      (error (c)
        (set-resource-state r :unloaded)
        (error c)))
    (set-resource-state r :prepared)
    (unless (resource-background-load-p r)
      (resource-fire-preparing-complete r))))

(defmethod resource-load :around ((r resource) backgroundp)
  (unless (and (resource-background-load-p r) (not backgroundp))
    (let ((oldstate (resource-state r)))
      (when (eq oldstate :preparing)
        (iter (while (eq (resource-state r) :preparing))
              (auto-lock r)
              (auto-unlock r))
        (setf oldstate (resource-state r)))
      (when (or (eq oldstate :unloaded) (eq oldstate :prepared) (eq oldstate :loading))
        (when (or (eq oldstate :loading) (not (resource-try-swap-state r oldstate :loading)))
          (iter (while (eq (resource-state r) :loading))
                (auto-lock r))
          (return-from resource-load)))
      (handler-case (with-auto-lock r
                      (cond ((resource-manual-load-p r)
                             (resource-do-pre-load r)
                             (if-let ((loader (resource-manual-loader r)))
                               (resource-loader-load-resource loader r)
                               (logmesg :trivial "WARNING: ~S instance '~S' was defined as manually loaded, but no manual loader was provided. This Resource will be lost if it has to be reloaded."
                                        (resource-loader-resource-type loader) (name r)))
                             (resource-do-post-load r))
                            (t
                             (when (eq oldstate :unloaded)
                               (resource-do-prepare r))
                             (resource-do-pre-load r)
                             (setf oldstate :prepared)
                             (when (eq (resource-groupname r) :autodetect-resource-group)
                               (resource-change-ownership r (resource-group-manager-find-group-containing-resource *resource-group-manager* (name r))))
                             (call-next-method)
                             (resource-do-post-load r)))
                      (setf (slot-value r 'size) (resource-compute-size r)))
        (error (c)
          (set-resource-state r :unloaded)
          (error c)))
      (set-resource-state r :loaded)
      (resource-set-state-dirty r)
      (when-let ((manager (resource-manager r)))
        (resource-manager-notify-resource-loaded manager r))
      (unless (resource-background-load-p r)
        (resource-fire-loading-complete r)))))

(defmethod resource-unload :around ((r resource))
  (let ((oldstate (resource-state r)))
    (when (and (or (eq oldstate :loaded) (eq oldstate :prepared))
               (resource-try-swap-state r oldstate :unloading))
      (with-auto-lock r
        (if (eq oldstate :prepared)
            (resource-unprepare r)
            (call-next-method)))
      (set-resource-state r :unloaded)
      (when (eq oldstate :loaded)
        (resource-manager-notify-resource-unloaded (resource-creator r) r))
      (resource-fire-unloading-complete r))))

(defmethod resource-reload ((r resource))
  (with-auto-lock r
    (when (eq :loaded (resource-state r))
      (resource-unload r)
      (resource-load r nil))))

(defmethod resource-touch ((r resource))
  (resource-load r nil)
  (when-let ((manager (resource-creator r)))
    (resource-manager-notify-resource-touched manager r)))

(defmethod resource-add-listener ((r resource) (l resource-listener))
  (with-lock (r listener)
    (appendf (slot-value r 'listeners) (list l))))

(defmethod resource-remove-listener ((r resource) (l resource-listener))
  (with-lock (r listener)
    (removef (slot-value r 'listeners) l :test #'eq)))

(defmethod resource-fire-preparing-complete ((r resource))
  (with-lock (r listener)
    (dolist (l (resource-listeners r))
      (when (resource-background-load-p r)
        (resource-listener-background-prepare-complete l r))
      (resource-listener-prepare-complete l r))))

(defmethod resource-fire-loading-complete ((r resource))
  (with-lock (r listener)
    (dolist (l (resource-listeners r))
      (when (resource-background-load-p r)
        (resource-listener-background-load-complete l r))
      (resource-listener-load-complete l r))))

(defmethod resource-fire-unloading-complete ((r resource))
  (with-lock (r listener)
    (dolist (l (resource-listeners r))
      (resource-listener-unload-complete l r))))
