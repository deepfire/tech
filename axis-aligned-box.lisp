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

(defstruct (vector2 (:conc-name v2-) (:constructor make-v2 (x y)))
  (x 0 :type real)
  (y 0 :type real))

(defstruct (vector3 (:conc-name v3-) (:constructor make-v3 (x y z)))
  (x 0 :type real)
  (y 0 :type real)
  (z 0 :type real))

(defstruct (vector4 (:conc-name v4-) (:constructor make-v4 (x y z w)))
  (x 0 :type real)
  (y 0 :type real)
  (z 0 :type real)
  (w 0 :type real))

(defun v2= (v1 v2)
  (and (= (v2-x v1) (v2-x v2))
       (= (v2-y v1) (v2-y v2))))
(defun v2+ (v1 v2)
  (make-v2 (+ (v2-x v1) (v2-x v2))
           (+ (v2-y v1) (v2-y v2))))
(defun v2- (v1 v2)
  (make-v2 (- (v2-x v1) (v2-x v2))
           (- (v2-y v1) (v2-y v2))))
(defun v2* (v1 scalar)
  (declare (type real scalar))
  (make-v2 (* (v2-x v1) scalar)
           (* (v2-y v1) scalar)))
(defun v2/ (v1 scalar)
  (declare (type real scalar))
  (assert (not (zerop scalar)))
  (let ((finv (/ 1 scalar)))
    (make-v2 (* (v2-x v1) finv)
             (* (v2-y v1) finv))))
(defun v2-neg (v2)
  (make-v2 (- (v2-x v2)) (- (v2-y v2))))
(defun v2-incf (v1 v2-or-real)
  (multiple-value-bind (dx dy) (if (realp v2-or-real)
                                   (values v2-or-real v2-or-real)
                                   (values (v2-x v2-or-real) (v2-y v2-or-real)))
    (incf (v2-x v1) dx)
    (incf (v2-y v1) dy)))
(defun v2-decf (v1 v2-or-real)
  (multiple-value-bind (dx dy) (if (realp v2-or-real)
                                   (values v2-or-real v2-or-real)
                                   (values (v2-x v2-or-real) (v2-y v2-or-real)))
    (decf (v2-x v1) dx)
    (decf (v2-y v1) dy)))
(defun v2-multf (v1 v2-or-real)
  (multiple-value-bind (dx dy) (if (realp v2-or-real)
                                   (values v2-or-real v2-or-real)
                                   (values (v2-x v2-or-real) (v2-y v2-or-real)))
    (setf (v2-x v1) (* dx (v2-x v1)))
    (setf (v2-y v1) (* dy (v2-y v1)))))
(defun v2-divf (v1 v2-or-real)
  (multiple-value-bind (dx dy) (if (realp v2-or-real)
                                   (values v2-or-real v2-or-real)
                                   (values (v2-x v2-or-real) (v2-y v2-or-real)))
    (assert (and (not (zerop dx)) (not (zerop dy))))
    (setf (v2-x v1) (/ dx (v2-x v1)))
    (setf (v2-y v1) (/ dy (v2-y v1)))))
(defun v2-length-squared (v2)
  (+ (* (v2-x v2) (v2-x v2)) (* (v2-y v2) (v2-y v2))))
(defun v2-distance-squared (v1 v2)
  (v2-length-squared (v2- v1 v2)))
(defun v2-midpoint (v1 v2)
  (make-v2 (* (+ (v2-x v1) (v2-x v2)) 0.5) (* (+ (v2-y v1) (v2-y v2)) 0.5)))
(defun v2< (v1 v2)
  (and (< (v2-x v1) (v2-x v2))
       (< (v2-y v1) (v2-y v2))))
(defun v2> (v1 v2)
  (and (> (v2-x v1) (v2-x v2))
       (> (v2-y v1) (v2-y v2))))
(defun v2-floorf (v1 v2)
  (setf (v2-x v1) (min (v2-x v1) (v2-x v2))
        (v2-y v1) (min (v2-y v1) (v2-y v2))))
(defun v2-zerop (v2)
  (< (v2-length-squared v2) 0.0000000000001))


(defun v2-length (v2)
  (sqrt (v2-length-squared v2)))

(defun v2-distance (v1 v2)
  (v2-length (v2- v1 v2)))

(defun v2-dot (v1 v2)
  (+ (* (v2-x v1) (v2-x v2)) (* (v2-y v1) (v2-y v2))))

(defun v2-normalizef (v2)
  (lret ((length (v2-length v2)))
    (when (> length 0.00000001)
      (let ((finv (/ 1 length)))
        (setf (v2-x v2) (* (v2-x v2) finv)
              (v2-y v2) (* (v2-y v2) finv))))))

(defun v2-ceilf (v1 v2)
  (setf (v2-x v1) (max (v2-x v1) (v2-x v2))
        (v2-y v1) (max (v2-y v1) (v2-y v2))))

(defun v2-perpendicular (v2)
  (make-v2 (- (v2-y v2)) (v2-x v2)))

(defun v2-cross (v1 v2)
  (- (* (v2-x v1) (v2-y v2)) (* (v2-y v1) (v2-x v2))))

(defun v2-random-deviant (v2 angle)
  (let* ((angle (* 2 angle (random pi)))
         (sina (sin angle))
         (cosa (cos angle)))
    (make-v2 (- (* cosa (v2-x v2)) (* sina (v2-y v2)))
             (+ (* sina (v2-x v2)) (* cosa (v2-y v2))))))

(defun v2-normalised (v2)
  (lret ((ret (copy-vector2 v2)))
    (v2-normalizef ret)))

(defun v2-reflect (v2 normal)
  (v2- v2 (v2-mult (v2-mult (v2-dot v2 normal) 2.0) normal)))

(defvar *v2-zero* (make-v2 0.0 0.0))
(defvar *v2-unit-x* (make-v2 1.0 0.0))
(defvar *v2-unit-y* (make-v2 0.0 1.0))
(defvar *v2-negative-unit-x* (make-v2 -1.0 0.0))
(defvar *v2-negative-unit-y* (make-v2 0.0 -1.0))
(defvar *v2-unit-scale* (make-v2 1.0 1.0))

(defun v3= (v1 v2)
  (and (= (v3-x v1) (v3-x v2))
       (= (v3-y v1) (v3-y v2))
       (= (v3-z v1) (v3-z v2))))

(defun v4= (v1 v2)
  (and (= (v4-x v1) (v4-x v2))
       (= (v4-y v1) (v4-y v2))
       (= (v4-z v1) (v4-z v2))
       (= (v4-w v1) (v4-w v2))))

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
(defstruct (axis-aligned-box (:conc-name aab-) (:constructor %make-axis-aligned-box (extent minimum maximum)))
  "A 3D box aligned with the x/y/z axes.

This class represents a simple box which is aligned with the
axes. Internally it only stores 2 points as the extremeties of
the box, one which is the minima of all 3 axes, and the other
which is the maxima of all 3 axes. This class is typically used
for an axis-aligned bounding box (AABB) for collision and
visibility determination."
  (extent nil :type (member :null :finite :infinite))
  (minumum nil :type vector3)
  (maximum nil :type vector3)
  (corners nil :type list))

(defun aab-set-null (aab)     (setf (aab-extent aab) :null))
(defun aab-set-finite (aab)   (setf (aab-extent aab) :finite))
(defun aab-set-infinite (aab) (setf (aab-extent aab) :infinite))
(defun aab-set-minimum (aab v3)
  (set-finite aab)
  (setf (aab-minimum aab) v3))
(defun aab-set-minimum* (aab x y z)
  (set-finite aab)
  (setf (v3-x (aab-minimum aab)) x
        (v3-y (aab-minimum aab)) y
        (v3-z (aab-minimum aab)) z))
(defun aab-set-maximum (aab v3)
  (set-finite aab)
  (setf (aab-maximum aab) v3))
(defun aab-set-maximum* (aab x y z)
  (set-finite aab)
  (setf (v3-x (aab-maximum aab)) x
        (v3-y (aab-maximum aab)) y
        (v3-z (aab-maximum aab)) z))
(defun aab-set-extents (aab min max)
  (aab-set-minimum aab min)
  (aab-set-maximum aab max))
(defun aab-set-extents* (aab min-x min-y min-z max-x max-y max-z)
  (aab-set-minimum* aab min-x min-y min-z)
  (aab-set-maximum* aab max-x max-y max-z))

(defun make-aab (&key extent origin)
  (multiple-value-bind (extent minimum maximum)
      (cond (extent (values extent (make-v3 -0.5 -0.5 -0.5) (make-v3 0.5 0.5 0.5)))
            (origin (values (aab-extent origin) min max))
            (t      (values :null (make-v3 -0.5 -0.5 -0.5) (make-v3 0.5 0.5 0.5))))
    (%make-axis-aligned-box extent minimum maximum)))

(defun make-aab* (min max)
  (%make-axis-aligned-box :finite min max))

(defun make-aab** (x-min y-min z-min x-max y-max z-max)
  (%make-axis-aligned-box :finite (make-v3 x-min y-min z-min) (make-v3 x-max y-max z-max)))

(defun aab-get-all-corners (aab)
  (unless (eq (aab-extent aab) :finite)
    (error "~@<Can't get corners of a null or infinite AAB.~:@>"))
  (setf (aab-corners aab)
        (make-array 8 :initial-contents
                    (list (aab-minimum aab)
                          (make-v3 (v3-x (aab-minimum aab)) (v3-y (aab-maximum aab)) (v3-z (aab-minimum aab)))
                          (make-v3 (v3-x (aab-maximum aab)) (v3-y (aab-maximum aab)) (v3-z (aab-minimum aab)))
                          (make-v3 (v3-x (aab-maximum aab)) (v3-y (aab-minimum aab)) (v3-z (aab-minimum aab)))
                          (aab-maximum aab)
                          (make-v3 (v3-x (aab-minimum aab)) (v3-y (aab-maximum aab)) (v3-z (aab-maximum aab)))
                          (make-v3 (v3-x (aab-minimum aab)) (v3-y (aab-minimum aab)) (v3-z (aab-maximum aab)))
                          (make-v3 (v3-x (aab-maximum aab)) (v3-y (aab-minimum aab)) (v3-z (aab-maximum aab)))))))