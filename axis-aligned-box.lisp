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

;;;;
;;;;     1-----2
;;;;    /|    /|
;;;;   / |   / |
;;;;  5-----4  |
;;;;  |  0--|--3
;;;;  | /   | /
;;;;  |/    |/
;;;;  6-----7
;;;;
(defclass axis-aligned-box ()
  ((extent :accessor aab-extent :type (member :null :finite :infinite))
   (minumum :accessor aab-minimum :type vector3)
   (maximum :accessor aab-maximum :type vector3)
   (corners :accessor aab-corners))
  (:documentation
   "A 3D box aligned with the x/y/z axes.

This class represents a simple box which is aligned with the
axes. Internally it only stores 2 points as the extremeties of
the box, one which is the minima of all 3 axes, and the other
which is the maxima of all 3 axes. This class is typically used
for an axis-aligned bounding box (AABB) for collision and
visibility determination."))

(defgeneric aab-set-null (aab))
(defgeneric aab-set-infinite (aab))
(defgeneric aab-set-extents (aab min max))
(defgeneric aab-set-extents* (aab min-x min-y min-z max-x max-y max-z))

(defmethod initialize-instance :after ((o axis-aligned-box) &key extent &allow-other-keys)
  (cond ((null extent)
         (aab-set-minimum o -0.5 -0.5 -0.5)
         (aab-set-maximum o 0.5 0.5 0.5))))
