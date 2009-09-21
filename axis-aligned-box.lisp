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

(defmacro inst (form &rest repertoire-set)
  `(iter (repeat arity)
         (for acc in accs)
         ,@(when repertoire-set `((for repvar in ',repertoire-set)))
         (collect ,form)))
(defmacro minst (&rest forms)
  `(iter (repeat arity)
         (for acc in accs)
         (appending (list ,@forms))))

(defmacro frob-vector-functions (arity)
  (flet ((name (string) (format-symbol t string arity)))
    (let ((maker (name "MAKE-V~D"))
          (accs (subseq (list (name "V~D-X") (name "V~D-Y") (name "V~D-Z") (name "V~D-W")) 0 arity)))
      `(progn
         (defun ,(name "V~D=") (v1 v2)
           (and ,@(inst `(= (,acc v1) (,acc v2)))))
         (defun ,(name "V~D+") (v1 v2)
           (,maker ,@(inst `(+ (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-") (v1 v2)
           (,maker ,@(inst `(- (,acc v1) (,acc v2)))))
         (defun ,(name "V~D*") (v scalar)
           (declare (type real scalar))
           (,maker ,@(inst `(* (,acc v) scalar))))
         (defun ,(name "V~D/") (v scalar)
           (declare (type real scalar))
           (assert (not (zerop scalar)))
           (let ((finv (/ 1 scalar)))
             (,maker ,@(inst `(* (,acc v) finv)))))
         (defun ,(name "V~D-NEGF") (v)
           (,maker ,@(inst `(- (,acc v)))))
         (defun ,(name "V~D-INCF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             ,@(inst `(incf (,acc v1) ,repvar) dx dy dz dw)))
         (defun ,(name "V~D-DECF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             ,@(inst `(decf (,acc v1) ,repvar) dx dy dz dw)))
         (defun ,(name "V~D-MULTF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             ,@(inst `(setf (,acc v1) (* (,acc v1) ,repvar)) dx dy dz dw)))
         (defun ,(name "V~D-DIVF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             (assert (and ,@(inst `(not (zerop ,repvar)) dx dy dz dw)))
             ,@(inst `(setf (,acc v1) (/ (,acc v1) ,repvar)) dx dy dz dw)))
         (defun ,(name "V~D-LENGTH-SQUARED") (v)
           (+ ,@(inst `(* (,acc v) (,acc v)))))
         (defun ,(name "V~D-DISTANCE-SQUARED") (v1 v2)
           (,(name "V~D-LENGTH-SQUARED") (v2- v1 v2)))
         (defun ,(name "V~D-LENGTH") (v)
           (sqrt (,(name "V~D-LENGTH-SQUARED") v)))
         (defun ,(name "V~D-DISTANCE") (v1 v2)
           (,(name "V~D-LENGTH") (,(name "V~D-") v1 v2)))
         (defun ,(name "V~D-NORMALIZEF") (v)
           (lret ((length ( ,(name "V~D-LENGTH") v)))
             (when (> length 0.00000001)
               (let ((finv (/ 1 length)))
                 (setf ,@(minst `(,acc v) `(* (,acc v) finv)))))))
         (defun ,(name "V~D-NORMALIZED") (v2)
           (lret ((ret (,(name "COPY-VECTOR~D") v2)))
             (,(name "V~D-NORMALIZEF") ret)))
         (defun ,(name "V~D-FLOORF") (v1 v2)
           (setf ,@(minst `(,acc v1) `(min (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-CEILF") (v1 v2)
           (setf ,@(minst `(,acc v1) `(max (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-MIDPOINT") (v1 v2)
           (,maker ,@(inst `(* (+ (,acc v1) (,acc v2)) 0.5))))
         (defun ,(name "V~D<") (v1 v2)
           (and ,@(inst `(< (,acc v1) (,acc v2)))))
         (defun ,(name "V~D>") (v1 v2)
           (and ,@(inst `(> (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-DOT") (v1 v2)
           (+ ,@(inst `(* (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-REFLECT") (v normal)
           (,(name "V~D-") v (,(name "V~D*") (,(name "V~D*") (,(name "V~D-DOT") v normal) 2.0) normal)))
         (defun ,(name "V~D-ZEROP") (v)
           (< (,(name "V~D-LENGTH-SQUARED") v) 0.0000000000001))))))

(frob-vector-functions 2)
(frob-vector-functions 3)

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

(defvar *v2-zero* (make-v2 0.0 0.0))
(defvar *v2-unit-x* (make-v2 1.0 0.0))
(defvar *v2-unit-y* (make-v2 0.0 1.0))
(defvar *v2-negative-unit-x* (make-v2 -1.0 0.0))
(defvar *v2-negative-unit-y* (make-v2 0.0 -1.0))
(defvar *v2-unit-scale* (make-v2 1.0 1.0))

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
  (minimum nil :type vector3)
  (maximum nil :type vector3)
  (corners nil :type (or null simple-vector)))

(defun aab-null-p (aab)       (eq (aab-extent aab) :null))
(defun aab-finite-p (aab)     (eq (aab-extent aab) :finite))
(defun aab-infinite-p (aab)   (eq (aab-extent aab) :infinite))
(defun aab-set-null (aab)     (setf (aab-extent aab) :null))
(defun aab-set-finite (aab)   (setf (aab-extent aab) :finite))
(defun aab-set-infinite (aab) (setf (aab-extent aab) :infinite))
(defun aab-set-minimum (aab v3)
  (aab-set-finite aab)
  (setf (aab-minimum aab) v3))
(defun aab-set-minimum* (aab x y z)
  (aab-set-finite aab)
  (setf (v3-x (aab-minimum aab)) x
        (v3-y (aab-minimum aab)) y
        (v3-z (aab-minimum aab)) z))
(defun aab-set-maximum (aab v3)
  (aab-set-finite aab)
  (setf (aab-maximum aab) v3))
(defun aab-set-maximum* (aab x y z)
  (aab-set-finite aab)
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
            (origin (values (aab-extent origin) (aab-minimum origin) (aab-maximum origin)))
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

(defun aab-get-corner (aab corner)
  (case corner
    (:far-left-bottom   (aab-minimum aab))
    (:far-left-top      (make-v3 (v3-x (aab-minimum aab)) (v3-y (aab-maximum aab)) (v3-z (aab-minimum aab))))
    (:far-right-top     (make-v3 (v3-x (aab-maximum aab)) (v3-y (aab-maximum aab)) (v3-z (aab-minimum aab))))
    (:far-right-bottom  (make-v3 (v3-x (aab-maximum aab)) (v3-y (aab-minimum aab)) (v3-z (aab-minimum aab))))
    (:near-right-bottom (make-v3 (v3-x (aab-maximum aab)) (v3-y (aab-minimum aab)) (v3-z (aab-maximum aab))))
    (:near-left-bottom  (make-v3 (v3-x (aab-minimum aab)) (v3-y (aab-minimum aab)) (v3-z (aab-maximum aab))))
    (:near-left-top     (make-v3 (v3-x (aab-minimum aab)) (v3-y (aab-maximum aab)) (v3-z (aab-maximum aab))))
    (:near-right-top    (aab-maximum aab))))

(defun merge-aab (aab1 aab2)
  (cond ((or (aab-infinite-p aab1) (aab-null-p aab2)) aab1)
        ((aab-infinite-p aab2) (aab-set-infinite aab1))
        ((aab-null-p aab1) (aab-set-extents aab1 (aab-minimum aab2) (aab-maximum aab2)))
        (t )))