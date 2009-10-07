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
;;; Resource
;;;
(defcassable resource-state value)

(defclass resource (named auto-locked)
  ((state :initarg :load-state :type resource-state)
   (statecount :reader resource-statecount)
   (creator :reader resource-creator :type (or null resource-manager) :initarg :creator)
   (groupname :reader resource-groupname :type symbol :initarg :groupname)
   (handle :accessor resource-handle :initarg :handle)
   (size :reader resource-size :initarg :size)
   
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
(defgeneric resource-load (resource &optional backgroundp))
(defgeneric resource-do-post-load (resource))
(defgeneric resource-fire-loading-complete (resource))

(defgeneric resource-unload (resource))
(defgeneric resource-fire-unloading-complete (resource))

(defgeneric resource-reload (resource))
(defgeneric resource-touch (resource))

(defgeneric resource-set-parameter-list (resource parameter-list))

(defgeneric resource-reloadable-p (resource)
  (:method ((o resource)) (or (not (resource-manual-load-p o)) (resource-manual-loader o))))

(defclass manual-resource-loader ()
  ())

(defclass resource-loader ()
  ((resource-type :accessor resource-loader-resource-type :initarg :resource-type)))

(defclass manual-resource-loader (resource-loader)  ())

(defgeneric resource-loader-prepare-resource (loader resource))
(defgeneric resource-loader-load-resource (loader resource))

(defclass resource-group-listener () ())

(defgeneric resource-group-listener-group-scripting-started (listener group-name script-count))
(defgeneric resource-group-listener-parse-started (listener script-name skip-this-script-p))
(defgeneric resource-group-listener-parse-ended (listener script-name skipped-p))
(defgeneric resource-group-listener-group-scripting-ended (listener group-name))
(defgeneric resource-group-listener-group-prepare-started (listener group-name resource-count))
(defgeneric resource-group-listener-prepare-started (listener resource))
(defgeneric resource-group-listener-prepare-ended (listener))
(defgeneric resource-group-listener-world-geometry-prepare-stage-started (listener description))
(defgeneric resource-group-listener-world-geometry-prepare-stage-ended (listener ))
(defgeneric resource-group-listener-group-prepare-ended (listener group-name))
(defgeneric resource-group-listener-group-load-started (listener group-name resource-count))
(defgeneric resource-group-listener-resource-load-started (listener resource))
(defgeneric resource-group-listener-world-geometry-stage-started (listener description))
(defgeneric resource-group-listener-description (listener))
(defgeneric resource-group-listener-group-load-ended (listener group-name))

(defclass resource-loading-listener () ())

(defgeneric resource-loading-listener-resource-loading (listener name group-name resource))
(defgeneric resource-loading-listener-stream-opened (listener name group-name resource data-stream))
(defgeneric resource-loading-listener-resource-collision (listener resource resource-manager))

(defstruct resource-declaration
  (name nil :type (or null symbol))
  (type nil :type (or null symbol))
  (manual-loader nil :type (or null manual-resource-loader))
  (params nil :type list))

(defstruct resource-location
  (archive nil :type (or null archive))
  (recursivep nil :type boolean))

(defclass scene-manager () ())

(defgeneric scene-manager-estimate-world-geometry (scene-manager world-geometry))
(defgeneric scene-manager-prepare-world-geometry (scene-manager geometry))
(defgeneric scene-manager-set-world-geometry (scene-manager geometry))

(defstruct resource-group
  (status :uninitialised)
  (name nil :type symbol)
  (auto-lock (bordeaux-threads:make-recursive-lock))
  (status-lock (bordeaux-threads:make-recursive-lock))
  (locations nil :type list)
  (location-index-case-sensitive (make-hash-table :test 'equal) :type hash-table)
  (location-index-case-insensitive (make-hash-table :test 'equalp) :type hash-table)
  (declarations nil :type list)
  (load-order nil :type list)
  (world-geometry nil :type symbol)
  (world-geometry-scene-manager nil :type scene-manager)
  (global-p nil :type boolean))

(defun resource-group-index (group archive filename))
(defun resource-group-deindex (group archive &optional filename))

(defmethod name ((o resource-group))
  (resource-group-name o))

(defmethod auto-lock ((o resource-group))
  (bordeaux-threads:acquire-lock (resource-group-auto-lock o)))

(defmethod auto-unlock ((o resource-group))
  (bordeaux-threads:release-lock (resource-group-auto-lock o)))

(defclass resource-group-manager (auto-locked)
  ((default-group-name :accessor resource-group-manager-default-group-name :initarg :default-group-name)
   (internal-group-name :accessor resource-group-manager-internal-group-name :initarg :internal-group-name)
   (bootstrap-group-name :accessor resource-group-manager-bootstrap-group-name :initarg :bootstrap-group-name)
   (autodetect-group-name :accessor resource-group-manager-autodetect-group-name :initarg :autodetect-group-name)
   (resource-declarations :accessor resource-group-manager-resource-declarations :initarg :resource-declarations)
   (manager-map :initarg :manager-map)
   (locations :initarg :locations)
   (load-order :accessor resource-group-manager-load-order :initarg :load-order)
   (group-listeners :accessor resource-group-manager-group-listeners :initarg :group-listeners)
   (loading-listener :accessor resource-group-manager-loading-listener :initarg :loading-listener)
   (location-index :accessor resource-group-manager-location-index :initarg :location-index)
   (load/unload-list :accessor resource-group-manager-load/unload-list :initarg :load/unload-list)
   (group-map :accessor resource-group-manager-group-map :initarg :group-map)
   (world-group-name :accessor resource-group-manager-world-group-name :initarg :world-group-name)
   (current-group :accessor resource-group-manager-current-group :initarg :current-group))
  (:default-initargs
   :default-group-name :general :internal-group-name :internal :bootstrap-group-name :bootstrap :autodetect-group-name :autodetect
   :resource-declarations nil
   :manager-map (make-hash-table :test 'eq)
   :locations nil
   :load-order nil
   :group-listeners nil
   :loading-listener nil
   :location-index (make-hash-table :test 'equal)
   :load/unload-list nil
   :group-map (make-hash-table :test 'eq)
   :world-group-name nil
   :current-group nil))

(defconstant +resource-system-reference-count+ 3)
(defvar *resource-group-manager*)

(define-subcontainer group-manager-manager :container-slot manager-map :if-exists :error :type resource-manager
                     :iterator do-manager-managers :iterator-bind-key t :remover %remove-manager-manager)
(define-subcontainer %group-manager-group :container-slot group-map :if-exists :error :type resource-group
                     :iterator do-manager-groups :iterator-bind-key t :remover %remove-manager-group)

(defun group-manager-group (group-manager name &key (if-does-not-exist :error))
  (declare (type resource-group-manager group-manager) (symbol name))
  (with-auto-lock group-manager
    (%group-manager-group group-manager name :if-does-not-exist if-does-not-exist)))

(defgeneric resource-group-manager-parse-group-scripts (manager group))
(defgeneric resource-group-manager-create-declared-resources (manager group))
(defgeneric resource-group-manager-add-created-resource (manager resource group))
(defgeneric resource-group-manager-drop-group-contents (manager group))
(defgeneric resource-group-manager-delete-group (manager group))
(defgeneric resource-group-manager-find-group-containing-resource (manager filename))
(defgeneric resource-group-manager-fire-group-scripting-started (manager group-name script-count))
(defgeneric resource-group-manager-fire-script-started (manager script-name skip-script-p))
(defgeneric resource-group-manager-fire-script-ended (manager skipped))
(defgeneric resource-group-manager-fire-group-scripting-ended (manager group-name))
(defgeneric resource-group-manager-fire-group-load-started (manager group-name resource-count))
(defgeneric resource-group-manager-fire-resource-load-started (manager resource))
(defgeneric resource-group-manager-fire-resource-load-ended (manager))
(defgeneric resource-group-manager-fire-group-load-ended (manager group-name))
(defgeneric resource-group-manager-fire-group-prepare-started (manager group-name resource-count))
(defgeneric resource-group-manager-fire-resource-prepare-started (manager resource))
(defgeneric resource-group-manager-fire-resource-prepare-ended (manager))
(defgeneric resource-group-manager-fire-group-prepare-ended (manager group-name))

(defgeneric resource-group-manager-create-group (manager group-name &optional in-global-pool-p))
(defgeneric resource-group-manager-initialise-group (manager group-name))
(defgeneric resource-group-manager-initialise-all-groups (manager))
(defgeneric resource-group-manager-prepare-group (manager group-name &optional prepare-main-resources prepare-world-geometry))
(defgeneric resource-group-manager-load-group (manager group-name &optional load-main-resources load-world-geometry))
(defgeneric resource-group-manager-unload-group (manager group-name &optional reloadable-only))
(defgeneric resource-group-manager-unload-unreferenced-group-resources (manager group-name &optional reloadable-only))
(defgeneric resource-group-manager-clear-group (manager group-name))
(defgeneric resource-group-manager-destroy-group (manager group-name))
(defgeneric resource-group-manager-group-initialised-p (manager group-name))
(defgeneric resource-group-manager-group-loaded-p (manager group-name))
(defgeneric resource-group-manager-add-location (manager name type &optional group-name recursivep))
(defgeneric resource-group-manager-remove-location (manager name &optional group-name))
(defgeneric resource-group-manager-location-exists-p (manager name &optional group-name))
(defgeneric resource-group-manager-declare-resource (manager name type &key group-name loader load-params))
(defgeneric resource-group-manager-undeclare-resource (manager name group-name))
(defgeneric resource-group-manager-open-resource (manager name &key group-name search-groups-if-not-found resource-being-loaded))
(defgeneric resource-group-manager-open-resources (manager pattern &optional group-name))
(defgeneric resource-group-manager-list-resource-names (manager group-name &optional dirs))
(defgeneric resource-group-manager-list-resource-file-info (manager group-name &optional dirs))
(defgeneric resource-group-manager-find-resource-names (manager group-name pattern &optional dirs))
(defgeneric resource-group-manager-resource-exists-p (manager group filename))
(defgeneric resource-group-manager-find-resource-file-info (manager group-name pattern &optional dirs))
(defgeneric resource-group-manager-resource-modified-time (manager group-name filename))
(defgeneric resource-group-manager-list-resource-locations (manager group-name))
(defgeneric resource-group-manager-find-resource-locations (manager group-name pattern))
(defgeneric resource-group-manager-create-resource (manager filename &key group-name overwrite location-pattern))
(defgeneric resource-group-manager-delete-resource (manager filename &key group-name location-pattern))
(defgeneric resource-group-manager-delete-matching-resource (manager file-pattern &key group-name location-pattern))
(defgeneric resource-group-manager-add-group-listener (manager listener))
(defgeneric resource-group-manager-remove-group-listener (manager listener))
(defgeneric resource-group-manager-link-world-geometry-to-group (manager group-name world-geometry scene-manager))
(defgeneric resource-group-manager-unlink-world-geometry-from-group (manager group-name))
(defgeneric resource-group-manager-group-in-global-pool-p (manager group-name))
(defgeneric resource-group-manager-shutdown-all (manager))
(defgeneric resource-group-manager-register-resource-manager (manager resource-type manager))
(defgeneric resource-group-manager-unregister-resource-manager (manager resource-type))
(defgeneric resource-group-manager-register-script-loader (manager loader))
(defgeneric resource-group-manager-unregister-script-loader (manager loader))
(defgeneric resource-group-manager-find-script-loader (manager pattern))
(defgeneric resource-group-manager-manager-by-type (manager type))
(defgeneric resource-group-manager-notify-resource-created (manager resource))
(defgeneric resource-group-manager-notify-resource-removed (manager resource))
(defgeneric resource-group-manager-notify-group-changed (manager old-group resource))
(defgeneric resource-group-manager-notify-all-resources-removed (manager))
(defgeneric resource-group-manager-notify-world-geometry-stage-started (manager description))
(defgeneric resource-group-manager-notify-world-geometry-stage-ended (manager))
(defgeneric resource-group-manager-get-groups (manager))
(defgeneric resource-group-manager-get-resource-declaration-list (manager group-name))
(defgeneric resource-group-manager-get-resource-location-list (manager group-name))
(defgeneric resource-group-manager-set-loading-listener (manager listener))
(defgeneric resource-group-manager-get-loading-listener (manager))

(defmethod initialize-instance :after ((o resource-group-manager) &rest rest)
  (declare (ignore rest))
  (resource-group-manager-create-group o :general)
  (resource-group-manager-create-group o :internal)
  (resource-group-manager-create-group o :autodetect)
  (setf (resource-group-manager-world-group-name o) :general))

(defmethod free ((o resource-group-manager))
  (do-manager-groups (nil g o)
    (resource-group-manager-delete-group o g))
  (clrhash (slot-value o 'group-map)))

(defmethod resource-group-manager-create-group ((o resource-group-manager) name &optional (in-global-pool-p t))
  (with-auto-lock o
    (logmesg :trivial "Creating resource group ~A" name)
    (setf (%group-manager-group o name) (make-resource-group :name name :global-p in-global-pool-p))))

(defun execute-with-group-manager-lock (manager name group-exit-state fn format-control &rest format-args)
  (declare (resource-group-manager manager) (symbol name) (function fn) (string format-control))
  (with-auto-lock manager
    (apply #'logmesg :trivial (concatenate 'string "Beginning " format-control) format-args)
    (let ((group (group-manager-group manager name)))
      (with-auto-lock group
        (setf (resource-group-manager-current-group manager) group)
        (funcall fn group)
        (when group-exit-state
          (setf (resource-group-status group) group-exit-state))
        (setf (resource-group-manager-current-group manager) nil)
        (apply #'logmesg :trivial (concatenate 'string "Finished " format-control) format-args)))))

(defmacro with-group-manager-group-action ((manager (group name &optional group-exit-state) format-control &rest format-args) &body body)
  `(execute-with-group-manager-lock ,manager ,name ,group-exit-state (lambda (,group) ,@body) ,format-control ,@format-args))

(defmethod resource-group-manager-initialise-group ((o resource-group-manager) name)
  (with-group-manager-group-action (o (g name :initialised) "initialisation of resource group ~A" name)
    (unless (eq (resource-group-status g) :uninitialised)
      (error "~@<Trying to initialise group ~A, but it is already initialised.~:@>" name))
    (setf (resource-group-status g) :initialising)
    (resource-group-manager-parse-group-scripts o g)
    (resource-group-manager-create-declared-resources o g)))

(defmethod resource-group-manager-prepare-group ((o resource-group-manager) name &optional (prepare-main-resources t) (prepare-world-geometry t))
  (with-group-manager-group-action (o (g name) "preparation of resource group ~A - Resources: ~S - World Geometry: ~S" name prepare-main-resources prepare-world-geometry)
    (let ((resource-count (+ (if prepare-main-resources
                                 (iter (for (nil . r) in (resource-group-load-order g)) (sum (resource-size r)))
                                 0)
                             (if-let ((scene-manager (and prepare-world-geometry (resource-group-world-geometry-scene-manager g))))
                               (scene-manager-estimate-world-geometry scene-manager (resource-group-world-geometry g))
                               0))))
      (resource-group-manager-fire-resource-group-prepare-started o name resource-count)
      (when prepare-main-resources
        (iter (for (nil . resource-list) in (resource-group-load-order g))
              (iter (for r in resource-list)
                    (resource-group-manager-fire-resource-prepare-started o r)
                    (resource-prepare r)
                    (resource-group-manager-fire-resource-prepare-ended o)
                    (when (eq name (resource-groupname r))
                      (logmesg :trivial "Resource ~A changed group ~A -> ~A" (name r) name (resource-groupname r))))))
      (when-let ((m (and prepare-world-geometry (resource-group-world-geometry-scene-manager g))))
        (scene-manager-prepare-geometry m (resource-group-world-geometry g)))
      (resource-group-manager-fire-group-prepare-ended o name))))

(defmethod resource-group-manager-load-group ((o resource-group-manager) name &optional (load-main-resources t) (load-world-geometry t))
  "Notice how this one is nearly identical to the above -- modulo status setting at the end. Meh.."
  (with-group-manager-group-action (o (g name :loaded) "load of resource group ~A - Resources: ~S - World Geometry: ~S" name load-main-resources load-world-geometry)
    (let ((resource-count (+ (if load-main-resources
                                 (iter (for (nil . r) in (resource-group-load-order g)) (sum (resource-size r)))
                                 0)
                             (if-let ((scene-manager (and load-world-geometry (resource-group-world-geometry-scene-manager g))))
                               (scene-manager-estimate-world-geometry scene-manager (resource-group-world-geometry g))
                               0))))
      (resource-group-manager-fire-resource-group-load-started o name resource-count)
      (when load-main-resources
        (iter (for (nil . resource-list) in (resource-group-load-order g))
              (iter (for r in resource-list)
                    (resource-group-manager-fire-resource-load-started o r)
                    (resource-load r)
                    (resource-group-manager-fire-resource-load-ended o)
                    (when (eq name (resource-groupname r))
                      (logmesg :trivial "Resource ~A changed group ~A -> ~A" (name r) name (resource-groupname r))))))
      (when-let ((m (and load-world-geometry (resource-group-world-geometry-scene-manager g))))
        (scene-manager-set-world-geometry m (resource-group-world-geometry g)))
      (resource-group-manager-fire-group-load-ended o name))))

(defmethod resource-group-manager-unload-group ((o resource-group-manager) name &optional (reloadable-only t))
  (with-group-manager-group-action (o (g name :initialised) "unload of resource group ~A" name)
    (iter (for (nil . resource-list) in (reverse (resource-group-load-order g)))
          (iter (for r in resource-list)
                (when (or (not reloadable-only) (resource-reloadable-p r))
                  (resource-unload r))))))

(defmethod resource-group-manager-unload-group-unreferenced ((o resource-group-manager) (name symbol) &optional (reloadable-only t))
  (with-group-manager-group-action (o (g name :initialised) "unload of unused resources in group ~A" name)
    (iter (for (nil . resource-list) in (reverse (resource-group-load-order g)))
          (iter (for r in resource-list)
                (broken "no use count information")
                (when (= +resource-system-reference-count+)
                  (when (or (not reloadable-only) (resource-reloadable-p r))
                    (resource-unload r)))))))

(defmethod resource-group-manager-clear-group ((o resource-group-manager) name)
  (with-group-manager-group-action (o (g name :uninitialised) "clearing resource group ~A" name)
    (resource-group-manager-drop-group-contents o g)))

(defmethod resource-group-manager-destroy-group ((o resource-group-manager) name)
  (with-group-manager-group-action (o (g name :uninitialised) "destruction of resource group ~A" name)
    (resource-group-manager-unload-group o name nil)
    (resource-group-manager-drop-group-contents o g)
    (resource-group-manager-delete-group o g)
    (%remove-manager-group o name)))

(defmethod resource-group-manager-group-initialised-p ((o resource-group-manager) (name symbol))
  (with-auto-lock o
    (let ((g (group-manager-group o name)))
      (not (or (eq (resource-group-status g) :uninitialised) (eq (resource-group-status g) :initialising))))))

(defmethod resource-group-manager-group-loaded-p ((o resource-group-manager) (name symbol))
  (with-auto-lock o
    (let ((g (group-manager-group o name)))
      (eq (resource-group-status g) :loaded))))

(defmethod resource-group-manager-location-exists-p ((o resource-group-manager) name &optional (group-name :general))
  (when-let ((g (group-manager-group o group-name :if-does-not-exist :continue)))
    (with-auto-lock g
      (iter (for l in (resource-group-locations g))
            (when (eq (name (resource-location-archive l)) name)
              (return t))))))

(defmethod resource-group-manager-add-location ((o resource-group-manager) name type &optional (group-name :general) recursivep)
  (let ((g (or (group-manager-group o group-name :if-does-not-exist :continue)
               (resource-group-manager-create-group o group-name))))
    (with-auto-lock g
      (let* ((archive (archive-manager-load *archive-manager* name type))
             (location (make-resource-location :archive archive :recursive recursivep)))
        (push location (resource-group-locations g))
        (iter (for filename in (archive-find archive "*" recursivep))
              (resource-group-index g archive filename))
        (logmesg :trivial "Added resource location '~A' of type '~A' to resource group '~A'~:[~; with recursive option~]" name type group-name recursivep)))))

(defmethod resource-group-manager-remove-location ((o resource-group-manager) name &optional (group-name :general))
  (let ((g (group-manager-group o group-name)))
    (with-auto-lock g
      (iter (for l in (resource-group-locations g))
            (for archive = (resource-location-archive l))
            (when (eq name (name archive))
              (resource-group-deindex g archive)
              (removef (resource-group-locations g) l)))
      (logmesg :trivial "Removed resource location '~A' from resource group '~A'" name group-name))))

(defclass resource-pool (named pool) ())

(defgeneric resource-pool-clear (pool)
  (:method ((o resource-pool))
    (do-pool-items (i o)
      (resource-manager-remove-resource (resource-creator i) (resource-handle i)))
    (pool-clear o)))

(defclass script-loader () ())

(defgeneric script-loader-get-script-patterns (script-loader))
(defgeneric script-loader-parse-script (script-loader data-stream group-name))
(defgeneric script-loader-get-loading-order (script-loader))

(defclass resource-manager (script-loader auto-locked)
  ((global-resource-map :type hash-table :initarg :global-resource-map)
   (grouped-resource-map :type hash-table :initarg :grouped-resource-map)
   (resource-handle-map :type hash-table :initarg :resource-handle-map)
   (resource-pool-map :type hash-table :initarg :resource-pool-map)
   (next-handle :initarg :next-handle)
   (memory-budget :reader resource-manager-memory-budget :initarg :memory-budget)
   (memory-usage :accessor resource-manager-memory-usage :initarg :memory-usage)
   (verbose :accessor resource-manager-verbose-p :initarg :verbose-p)
   (script-patterns :accessor resource-manager-script-patterns :initarg :script-patterns)
   (load-order :reader resource-manager-load-order :initarg :load-order)
   (resource-type :accessor resource-manager-resource-type :initarg :resource-type))
  (:default-initargs
   :load-order 0
   :next-handle 1
   :verbose t
   :memory-budget most-positive-fixnum
   :global-resource-map (make-hash-table :test 'eq)
   :grouped-resource-map (make-hash-table :test 'eq)
   :resource-handle-map (make-hash-table :test 'eq)
   :resource-pool-map (make-hash-table :test 'eq)))

(define-subcontainer manager-resource-by-handle :container-slot resource-handle-map :if-exists :error :type resource
                     :iterator do-manager-resources-by-handle :iterator-bind-key t :remover %remove-resource-handle)
(define-subcontainer %manager-resource-group :container-slot grouped-resource-map   :if-does-not-exist :continue :type hash-table
                     :iterator do-manager-resource-groups     :iterator-bind-key t :remover %remove-resource-group)
(define-subcontainer %manager-resource-pool :container-slot resource-pool-map       :if-does-not-exist :continue :type hash-table
                     :iterator do-manager-resource-pools      :iterator-bind-key t :remover %remove-resource-pool)
(define-subcontainer %manager-global-resource :container-slot global-resource-map   :if-exists :return-nil :if-does-not-exist :continue :type resource
                     :iterator do-manager-global-resources    :iterator-bind-key t :remover %remove-global-resource)
(defgeneric manager-resource-by-name (manager name group-name))

(defgeneric resource-manager-notify-resource-loaded (manager resource))
(defgeneric resource-manager-notify-resource-unloaded (manager resource))
(defgeneric resource-manager-notify-resource-touched (manager resource) (:method ((o resource-manager) r) (origtodo "R-M-NOTIFY-RESOURCE-TOUCHED")))
(defgeneric resource-manager-create-resource (manager name group manualp params &key &allow-other-keys))
(defgeneric resource-manager-prepare-resource (manager name group manualp params &key &allow-other-keys))
(defgeneric resource-manager-load-resource (manager name group manualp params &key &allow-other-keys))
(defgeneric resource-manager-unload-resource (manager resource))
(defgeneric resource-manager-unload-all (manager &optional reloadable-only))
(defgeneric resource-manager-reload-all (manager &optional reloadable-only))
(defgeneric resource-manager-unload-unreferenced (manager &optional reloadable-only))
(defgeneric resource-manager-reload-unreferenced (manager &optional reloadable-only))
(defgeneric resource-manager-get-next-handle (manager))
(defgeneric resource-manager-check-usage (manager) (:method ((o resource-manager)) (origtodo "R-M-CHECK-USAGE")))
(defgeneric resource-manager-add-resource (manager resource))
(defgeneric resource-manager-remove-resource (manager resource))
(defgeneric resource-manager-remove-all (manager))
(defgeneric resource-manager-create-or-retrieve-resource (manager name group-name manualp params &key &allow-other-keys))

(defgeneric resource-manager-pool-by-name (manager pool-name))
(defgeneric resource-manager-destroy-pool (manager pool))
(defgeneric resource-manager-destroy-all-pools (manager))

(defmethod free ((o resource-manager))
  (resource-manager-destroy-all-pools o)
  (resource-manager-remove-all o))

(defmethod resource-manager-get-next-handle ((o resource-manager))
  (with-auto-lock o
    (incf (slot-value o 'next-handle))))

(defmethod resource-manager-create-resource :around ((o resource-manager) name group-name manualp params &key resource-loader)
  (lret ((resource (call-next-method name group-name manualp params :resource-loader resource-loader :handle (resource-manager-get-next-handle o))))
    (when params
      (resource-set-parameter-list resource params))
    (resource-manager-add-resource o resource)
    (resource-group-manager-notify-resource-created *resource-group-manager* resource)))

(defmethod resource-manager-create-or-retrieve-resource ((o resource-manager) name group-name manualp params &key resource-loader)
  (with-auto-lock o
    (if-let ((r (manager-resource-by-name o name group-name)))
      (values r nil)
      (values (resource-manager-create-resource o name group-name manualp params :resource-loader resource-loader) t))))

(defmethod resource-manager-prepare-resource ((o resource-manager) name group-name manualp params &key resource-loader)
  (lret ((r (resource-manager-create-or-retrieve-resource o name group-name manualp params :resource-loader resource-loader)))
    (resource-prepare r)))

(defmethod resource-manager-load-resource ((o resource-manager) name group-name manualp params &key resource-loader backgroundp)
  (lret ((r (resource-manager-create-or-retrieve-resource o name group-name manualp params :resource-loader resource-loader)))
    (resource-load r backgroundp)))

(defmethod resource-manager-add-resource ((o resource-manager) r)
  (with-auto-lock o
    (unless (if (resource-group-manager-group-in-global-pool-p *resource-group-manager* (resource-groupname r))
                (setf (%manager-global-resource o (name r)) r)
                (puthash-unique (name r)
                                (or (%manager-resource-group o (resource-groupname r))
                                    (setf (%manager-resource-group o (resource-groupname r)) (make-hash-table :test #'eq)))
                                r))
      (if-let ((listener (resource-group-manager-get-loading-listener *resource-group-manager*)))
        (when (resource-loading-listener-resource-collision listener r o)
          (unless (if (resource-group-manager-group-in-global-pool-p *resource-group-manager* (resource-groupname r))
                      (setf (%manager-global-resource o (name r)) r)
                      (puthash-unique (name r)
                                      (%manager-resource-group o (resource-groupname r))
                                      r))
            (error "~@<Resource named ~S already exists.~:@>" (name r))))
        (return-from resource-manager-add-resource))
      (setf (manager-resource-by-handle o (resource-handle r)) r))))

(defmethod resource-manager-remove-resource ((o resource-manager) (r resource))
  (with-auto-lock o
    (if (resource-group-manager-group-in-global-pool-p *resource-group-manager* (resource-groupname r))
        (%remove-global-resource o (name r))
        (when-let ((group (%manager-resource-group o (resource-groupname r))))
          (remhash (name r) group)
          (when (zerop (hash-table-count group))
            (%remove-resource-group o (resource-groupname r)))))
    (%remove-resource-handle o (resource-handle r))
    (resource-group-manager-notify-resource-removed *resource-group-manager* r)))

(defmethod resource-manager-remove-resource ((o resource-manager) (name symbol))
  (when-let ((r (manager-resource-by-name o name)))
    (resource-manager-remove-resource o r)))

(defmethod resource-manager-remove-resource ((o resource-manager) (handle integer))
  (when-let ((r (manager-resource-by-handle o handle)))
    (resource-manager-remove-resource o r)))

(defmethod resource-manager-remove-all ((o resource-manager))
  (clrhash (slot-value o 'grouped-resource-map))
  (clrhash (slot-value o 'resource-handle-map))
  (clrhash (slot-value o 'resource-map))
  (resource-group-manager-notify-all-resources-removed *resource-group-manager*))

(defmethod resource-manager-remove-unreferenced ((o resource-manager) &optional reloadable-only)
  (broken "no use count information")
  (with-auto-lock o
    (do-manager-global-resources (nil r o)
      (when (= +resource-system-reference-count+)
        (when (or (not reloadable-only) (resource-reloadable-p r))
          (resource-manager-remove-resource o r))))))

(defmethod (setf resource-manager-memory-budget) (new-budget (o resource-manager))
  (setf (slot-value o 'memory-budget) new-budget)
  (resource-manager-check-usage o))

(defmethod resource-manager-unload-resource ((o resource-manager) (name symbol))
  (when-let ((r (manager-resource-by-name o name)))
    (resource-unload r)))

(defmethod resource-manager-unload-resource ((o resource-manager) (handle integer))
  (when-let ((r (manager-resource-by-handle o handle)))
    (resource-unload r)))

(defmethod resource-manager-unload-all ((o resource-manager) &optional reloadable-only)
  (with-auto-lock o
    (do-manager-global-resources (nil r o)
      (when (or (not reloadable-only) (resource-reloadable-p r))
        (resource-unload r)))))

(defmethod resource-manager-reload-all ((o resource-manager) &optional reloadable-only)
  (with-auto-lock o
    (do-manager-global-resources (nil r o)
      (when (or (not reloadable-only) (resource-reloadable-p r))
        (resource-reload r)))))

(defmethod resource-manager-unload-unreferenced ((o resource-manager) &optional reloadable-only)
  (broken "no use count information")
  (with-auto-lock o
    (do-manager-global-resources (nil r o)
      (when (= +resource-system-reference-count+)
        (when (or (not reloadable-only) (resource-reloadable-p r))
          (resource-unload r))))))

(defmethod resource-manager-reload-unreferenced ((o resource-manager) &optional reloadable-only)
  (broken "no use count information")
  (with-auto-lock o
    (do-manager-global-resources (nil r o)
      (when (= +resource-system-reference-count+)
        (when (or (not reloadable-only) (resource-reloadable-p r))
          (resource-reload r))))))

(defmethod manager-resource-by-name ((o resource-manager) (name symbol) (group-name symbol))
  (if (resource-group-manager-group-in-global-pool-p *resource-group-manager* group-name)
      (with-auto-lock o
        (or (%manager-global-resource o name)
            (when (eq group-name :autodetect-resource-group-name)
              (do-manager-resource-groups (nil group o)
                (when-let ((r (gethash name group)))
                  (return r))))))
      (with-auto-lock o
        (when-let ((group (%manager-resource-group o group-name)))
          (gethash name group)))))

(defmethod resource-manager-notify-resource-loaded ((o resource-manager) (r resource))
  (with-auto-lock o
    (incf (resource-manager-memory-usage o) (resource-size r))))

(defmethod resource-manager-notify-resource-unloaded ((o resource-manager) (r resource))
  (with-auto-lock o
    (decf (resource-manager-memory-usage o) (resource-size r))))

(defmethod resource-manager-pool-by-name ((o resource-manager) (name symbol))
  (with-auto-lock o
    (or (%manager-resource-pool o name)
        (setf (%manager-resource-pool o name) (make-instance 'resource-pool :name name)))))

(defmethod resource-manager-destroy-pool ((o resource-manager) (name symbol))
  (with-auto-lock o
    (%remove-resource-pool o name)))

(defmethod resource-manager-destroy-pool ((o resource-manager) (p resource-pool))
  (resource-manager-destroy-pool o (name p)))

(defmethod resource-manager-destroy-all-pools ((o resource-manager))
  (with-auto-lock o
    (clrhash (slot-value o 'resource-pool-map))))

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
      (resource-group-manager-notify-group-changed *resource-group-manager* old-group-name r))))

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

(defmethod resource-load :around ((r resource) &optional backgroundp)
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
      (when-let ((manager (resource-creator r)))
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
      (resource-load r))))

(defmethod resource-touch ((r resource))
  (resource-load r)
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
