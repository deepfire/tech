;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: ARCHIVE; Base: 10 -*-
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

(in-package :archive)

(defclass archive (named)
  ((read-only-p :accessor archive-read-only-p)
   (case-sensitive-p :accessor archive-case-sensitive-p)))

(defgeneric archive-init (archive))

(defgeneric archive-open (archive filename read-only-p))
(defgeneric archive-create (archive filename))
(defgeneric archive-remove (archive filename))
(defgeneric archive-exists-p (archive filename))
(defgeneric archive-modified-time (archive filename))

(defgeneric archive-list (archive &optional recursive dirs))
(defgeneric archive-list-file-info (archive &optional recursive dirs))
(defgeneric archive-find (archive pattern &optional recursive dirs))
(defgeneric archive-find-file-info (archive pattern &optional recursive dirs))

(defvar *archives* (make-hash-table :test 'equalp))

(defgeneric archive-load (filename type))
(defgeneric archive-unload (archive))

(defclass archive-manager () ())

(defgeneric archive-manager-load (archive-manager name type))

(defvar *archive-manager*)
