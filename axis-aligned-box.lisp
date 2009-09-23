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
;;;; Generic
;;;;
(defconstant epsilon 0.000001)
(defconstant epsilon-square (* epsilon epsilon))

(defun real= (x1 x2 tolerance)
  (declare (type real x1 x2 tolerance))
  (< (abs (- x1 x2)) tolerance))

(defgeneric volume (object))
(defgeneric scale (object v3))
(defgeneric scalef (object v3))
(defgeneric intersects-p (object-1 object-2))
(defgeneric contains-p (object-1 object-2))
(defgeneric equal-p (object-1 object-2))
(defgeneric setv (object-1 object-2))
(defgeneric mult (object-1 object-2))
(defgeneric mult-affine (object-1 object-2))

(defun ni () (error "~@<This function is not implemented.~:@>"))

;;;;
;;;; Quaternion
;;;;
(defstruct (quaternion (:conc-name q-) (:constructor make-q (x y z w)))
  (x 0 :type real)
  (y 0 :type real)
  (z 0 :type real)
  (w 0 :type real))

;;;;
;;;; Vector
;;;;
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

(defvar *v2-zero* (make-v2 0.0 0.0))
(defvar *v2-unit-x* (make-v2 1.0 0.0))
(defvar *v2-unit-y* (make-v2 0.0 1.0))
(defvar *v2-negative-unit-x* (make-v2 -1.0 0.0))
(defvar *v2-negative-unit-y* (make-v2 0.0 -1.0))
(defvar *v2-unit-scale* (make-v2 1.0 1.0))

(defvar *v3-zero* (make-v3 0.0 0.0 0.0))
(defvar *v3-unit-x* (make-v3 1.0 0.0 0.0))
(defvar *v3-unit-y* (make-v3 0.0 1.0 0.0))
(defvar *v3-unit-z* (make-v3 0.0 0.0 1.0))
(defvar *v3-negative-unit-x* (make-v3 -1.0 0.0 0.0))
(defvar *v3-negative-unit-y* (make-v3 0.0 -1.0 0.0))
(defvar *v3-negative-unit-z* (make-v3 0.0 0.0 -1.0))
(defvar *v3-unit-scale* (make-v3 1.0 1.0 1.0))

(defvar *v4-zero* (make-v4 0.0 0.0 0.0 0.0))
(defvar *v4-unit-x* (make-v4 1.0 0.0 0.0 1.0))
(defvar *v4-unit-y* (make-v4 0.0 1.0 0.0 1.0))
(defvar *v4-unit-z* (make-v4 0.0 0.0 1.0 1.0))
(defvar *v4-unit-w* (make-v4 0.0 0.0 0.0 1.0))
(defvar *v4-negative-unit-x* (make-v4 -1.0 0.0 0.0 1.0))
(defvar *v4-negative-unit-y* (make-v4 0.0 -1.0 0.0 1.0))
(defvar *v4-negative-unit-z* (make-v4 0.0 0.0 -1.0 1.0))
(defvar *v4-unit-scale* (make-v4 1.0 1.0 1.0 1.0))

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
         (defun ,(name "V~D-NEG") (v)
           (,maker ,@(inst `(- (,acc v)))))
         (defun ,(name "V~D-INCF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             ,@(inst `(incf (,acc v1) ,repvar) dx dy dz dw))
           v1)
         (defun ,(name "V~D-DECF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             ,@(inst `(decf (,acc v1) ,repvar) dx dy dz dw))
           v1)
         (defun ,(name "V~D-MULTF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             ,@(inst `(setf (,acc v1) (* (,acc v1) ,repvar)) dx dy dz dw))
           v1)
         (defun ,(name "V~D-DIVF") (v1 v2-or-real)
           (multiple-value-bind (,@(inst repvar dx dy dz dw)) (if (realp v2-or-real)
                                                                  (values ,@(inst `v2-or-real))
                                                                  (values ,@(inst `(,acc v2-or-real))))
             (assert (and ,@(inst `(not (zerop ,repvar)) dx dy dz dw)))
             ,@(inst `(setf (,acc v1) (/ (,acc v1) ,repvar)) dx dy dz dw))
           v1)
         (defun ,(name "V~D-LENGTH-SQUARED") (v)
           (+ ,@(inst `(* (,acc v) (,acc v)))))
         (defun ,(name "V~D-DISTANCE-SQUARED") (v1 v2)
           (,(name "V~D-LENGTH-SQUARED") (v2- v1 v2)))
         (defun ,(name "V~D-LENGTH") (v)
           (sqrt (,(name "V~D-LENGTH-SQUARED") v)))
         (defun ,(name "V~D-DISTANCE") (v1 v2)
           (,(name "V~D-LENGTH") (,(name "V~D-") v1 v2)))
         (defun ,(name "V~D-NORMALISEF") (v)
           (lret ((length ( ,(name "V~D-LENGTH") v)))
             (when (> length epsilon)
               (let ((finv (/ 1 length)))
                 (setf ,@(minst `(,acc v) `(* (,acc v) finv)))))))
         (defun ,(name "V~D-NORMALISE") (v2)
           (lret ((ret (,(name "COPY-VECTOR~D") v2)))
             (,(name "V~D-NORMALISEF") ret)))
         (defun ,(name "V~D-FLOOR") (v1 v2)
           (,maker ,@(inst `(min (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-CEIL") (v1 v2)
           (,maker ,@(inst `(max (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-FLOORF") (v1 v2)
           (setf ,@(minst `(,acc v1) `(min (,acc v1) (,acc v2))))
           v1)
         (defun ,(name "V~D-CEILF") (v1 v2)
           (setf ,@(minst `(,acc v1) `(max (,acc v1) (,acc v2))))
           v1)
         (defun ,(name "V~D-MIDPOINT") (v1 v2)
           (,maker ,@(inst `(* (+ (,acc v1) (,acc v2)) 0.5))))
         (defun ,(name "V~D<") (v1 v2)
           (and ,@(inst `(< (,acc v1) (,acc v2)))))
         (defun ,(name "V~D>") (v1 v2)
           (and ,@(inst `(> (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-DOT") (v1 v2)
           (+ ,@(inst `(* (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-ABS-DOT") (v1 v2)
           (+ ,@(inst `(abs (* (,acc v1) (,acc v2))))))
         (defun ,(name "V~D-REFLECT") (v normal)
           (,(name "V~D-") v (,(name "V~D*") (,(name "V~D*") (,(name "V~D-DOT") v normal) 2.0) normal)))
         (defun ,(name "V~D-ZEROP") (v)
           (< (,(name "V~D-LENGTH-SQUARED") v) epsilon-square))))))

(frob-vector-functions 2)
(frob-vector-functions 3)
(frob-vector-functions 4)

(defun v2-cross (v1 v2)
  (- (* (v2-x v1) (v2-y v2)) (* (v2-y v1) (v2-x v2))))

(defun v3-cross (v1 v2)
  (make-v3 (- (* (v3-y v1) (v3-z v2)) (* (v3-z v1) (v3-y v2)))
           (- (* (v3-z v1) (v3-x v2)) (* (v3-x v1) (v3-z v2)))
           (- (* (v3-x v1) (v3-y v2)) (* (v3-y v1) (v3-x v2)))))

(defun v2-perpendicular (v)
  (make-v2 (- (v2-y v))
           (v2-x v)))

(defun v3-perpendicular (v)
  (let ((perp (v3-cross v *v3-unit-x*)))
    (lret ((perp (if (< (v3-length-squared perp) epsilon-square)
                     (v3-cross v *v3-unit-y*)
                     perp)))
      (v3-normalisef perp))))

(defun v2-random-deviant (v angle)
  (let* ((angle (* 2 angle (random pi)))
         (sina (sin angle))
         (cosa (cos angle)))
    (make-v2 (- (* cosa (v2-x v)) (* sina (v2-y v)))
             (+ (* sina (v2-x v)) (* cosa (v2-y v))))))

(defun v3-random-deviant (v angle &optional (up (v3-perpendicular v)))
  ;; Rotate up vector by random amount around v
  (let* ((q1 (q<-angle-axis (* 2 pi (random 1.0)) v))
         (new-up (q* q1 up))
         ;; Finally rotate v by given angle around randomised up
         (q2 (q<-angle-axis angle new-up)))
    (q* q2 v)))

(defun v3-angle-between (v1 v2)
  (let ((lenproduct (max (* (v3-length v1) (v3-length v2)) epsilon-square)))
    (acos (clamp (/ (v3-dot v1 v2) lenproduct) -1.0 1.0))))

(defun v3-rotation-to (v1 v2 &optional fallback-axis)
  "Gets the shortest arc quaternion to rotate this vector to the destination
vector.
If you call this with a dest vector that is close to the inverse
of this vector, we will rotate 180 degrees around the 'fallbackAxis'
if specified, or a generated axis if not since in this case
ANY axis of rotation is valid.
Based on Stan Melax's article in Game Programming Gems."
  (let* ((v1n (v3-normalise v1))
         (v2n (v3-normalise v2))
         (ndot (v3-dot v1n v2n)))
    (when (>= ndot 1.0)
      (return-from v3-rotation-to *q-identity*))
    (if (< ndot (- epsilon 1.0))
        (if fallback-axis
            (q<-angle-axis pi fallback-axis)
            (let ((axis (v3-cross *v3-unit-x* v1)))
              (let ((axis (if (v3-zerop axis) (v3-cross *v3-unit-y* v1))))
                (q<-angle-axis pi (v3-normalise axis)))))
        (let* ((s (sqrt (* 2 (1+ ndot))))
               (invs (/ 1 s))
               (c (v3-cross v1n v2n)))
          (q-normalise (make-q (* (v3-x c) invs)
                               (* (v3-y c) invs)
                               (* (v3-z c) invs)
                               (* s 0.5)))))))

(defun v3-position-equalp (v1 v2 &optional (tolerance 0.001))
  (and (real= (v3-x v1) (v3-x v2) tolerance)
       (real= (v3-y v1) (v3-y v2) tolerance)
       (real= (v3-z v1) (v3-z v2) tolerance)))

(defun v3-position-closesp (v1 v2 &optional (tolerance 0.001))
  (<= (v3-distance-squared v1 v2) 
      (* tolerance (+ (v3-length-squared v1) (v3-length-squared v2)))))

(defun v3-direction-equalp (v1 v2 tolerance)
  (let ((angle (acos (v3-dot v1 v2))))
    (<= (abs angle) tolerance)))

;;;;
;;;; Trivial geometrics
;;;;
(defstruct (sphere (:constructor make-sphere (o r)))
  (o (make-v3 0.0 0.0 0.0) :type vector3)
  (r 0 :type real))

(defstruct (plane (:constructor make-plane (normal d)))
  (normal (make-v3 0.0 1.0 0.0) :type vector3)
  (d 0 :type real))

;;;;
;;;; Matrix
;;;;
;; All matrix code adapted from Wild Magic 0.2 Matrix math (free source code)
;; http://www.geometrictools.com/
;;
;; And, of course, re-adapted from Ogre.

;; NOTE.  The (x,y,z) coordinate system is assumed to be right-handed.
;; Coordinate axis rotation matrices are of the form
;;   RX =    1       0       0
;;           0     cos(t) -sin(t)
;;           0     sin(t)  cos(t)
;; where t > 0 indicates a counterclockwise rotation in the yz-plane
;;   RY =  cos(t)    0     sin(t)
;;           0       1       0
;;        -sin(t)    0     cos(t)
;; where t > 0 indicates a counterclockwise rotation in the zx-plane
;;   RZ =  cos(t) -sin(t)    0
;;         sin(t)  cos(t)    0
;;           0       0       1
;; where t > 0 indicates a counterclockwise rotation in the xy-plane.

(defstruct (matrix3 (:conc-name m3-) (:constructor make-m3 (&optional a)))
  "Storage format is concatenated set of columns:
c0x c0y c0z c1x c1y c1z c2x c2y c2z."
  (a (make-array 9 :element-type 'real) :type (simple-array real (9))))

(defun make-m3a ()
  (make-array 9 :element-type 'real))

(defun copy-m3 (m)
  (make-m3 (copy-array (the (simple-array real (9)) (m3-a m)))))

(defun make-m3* (m00 m10 m20 m01 m11 m21 m02 m12 m22)
  (make-m3 (make-array 9 :element-type 'real :initial-contents (vector m00 m10 m20 m01 m11 m21 m02 m12 m22))))

(defvar *m3-zero* (make-m3* 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
(defvar *m3-identity* (make-m3* 1.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 1.0))

(defun m3 (m r c)
  (aref (the (simple-array real (9)) (m3-a m)) (+ r (* 3 c))))

(defun (setf m3) (v m r c)
  (setf (aref (the (simple-array real (9)) (m3-a m)) (+ r (* 3 c))) v))

(defmethod equal-p ((m1 matrix3) (m2 matrix3))
  (equalp (the (simple-array real (9)) (m3-a m1))
          (the (simple-array real (9)) (m3-a m2))))

(defmethod setv ((dest matrix3) (src matrix3))
  (map-into (m3-a dest) #'identity (m3-a src)))

(defun m3+ (m1 m2)
  (make-m3 (map '(simple-array real (9)) #'+ (m3-a m1) (m3-a m2))))

(defun m3- (m1 m2)
  (make-m3 (map '(simple-array real (9)) #'- (m3-a m1) (m3-a m2))))

(defun m3-column (m c)
  (make-v3 (m3 m 0 c) (m3 m 1 c) (m3 m 2 c)))

(defun (setf m3-column) (v m c)
  (setf (m3 m 0 c) (v3-x v)
        (m3 m 1 c) (v3-y v)
        (m3 m 2 c) (v3-z v)))

(defun m3-scale (m)
  (let ((a (m3-a m)))
    (make-v3 (aref a 0) (aref a 4) (aref a 8))))

(defun (setf m3-scale) (v m)
  (let ((a (m3-a m)))
    (setf (aref a 0) (v3-x v)
          (aref a 4) (v3-y v)
          (aref a 8) (v3-z v))))

(defun m3-make-scale (v)
  (make-m3* (v3-x v) 0.0 0.0  0.0 (v3-y v) 0.0  0.0 0.0 (v3-z v)))

(defun m3-make-scale* (x y z)
  (make-m3* x 0.0 0.0  0.0 y 0.0  0.0 0.0 z))

(defmethod mult ((m1 matrix3) (m2 matrix3))
  (lret ((m (make-m3)))
    (dotimes (c 3)
      (dotimes (r 3)
        (setf (m3 m r c) (+ (* (m3 m1 r 0) (m3 m2 0 c))
                            (* (m3 m1 r 1) (m3 m2 1 c))
                            (* (m3 m1 r 2) (m3 m2 2 c))))))))

(defmethod mult ((m matrix3) (v vector3))
  (make-v3 (+ (* (m3 m 0 0) (v3-x v)) (* (m3 m 0 1) (v3-y v)) (* (m3 m 0 2) (v3-z v)))
           (+ (* (m3 m 1 0) (v3-x v)) (* (m3 m 1 1) (v3-y v)) (* (m3 m 1 2) (v3-z v)))
           (+ (* (m3 m 2 0) (v3-x v)) (* (m3 m 2 1) (v3-y v)) (* (m3 m 2 2) (v3-z v)))))

(defmethod mult ((v vector3) (m matrix3))
  (make-v3 (+ (* (m3 m 0 0) (v3-x v)) (* (m3 m 1 0) (v3-y v)) (* (m3 m 2 0) (v3-z v)))
           (+ (* (m3 m 0 1) (v3-x v)) (* (m3 m 1 1) (v3-y v)) (* (m3 m 2 1) (v3-z v)))
           (+ (* (m3 m 0 2) (v3-x v)) (* (m3 m 1 2) (v3-y v)) (* (m3 m 2 2) (v3-z v)))))

(defmethod mult ((m matrix3) (r real))
  (make-m3 (map '(simple-array real (9)) (lambda (x) (* x r)) (m3-a m))))

(defmethod mult ((r real) (m matrix3))
  (make-m3 (map '(simple-array real (9)) (lambda (x) (* x r)) (m3-a m))))

(defun m3-scalep (m)
  (not (and (real= (+ (* (m3 m 0 0) (m3 m 0 0)) (* (m3 m 1 0) (m3 m 1 0)) (* (m3 m 2 0) (m3 m 2 0))) 1.0 0.0001)
            (real= (+ (* (m3 m 0 1) (m3 m 0 1)) (* (m3 m 1 1) (m3 m 1 1)) (* (m3 m 2 1) (m3 m 2 1))) 1.0 0.0001)
            (real= (+ (* (m3 m 0 2) (m3 m 0 2)) (* (m3 m 1 2) (m3 m 1 2)) (* (m3 m 2 2) (m3 m 2 2))) 1.0 0.0001))))

(defun m3-neg (m)
  (make-m3 (map '(simple-array real (9)) #'- (m3-a m))))

(defun m3-transpose (m)
  (let ((a (m3-a m)))
    (make-m3* (aref a 0) (aref a 3) (aref a 6)
              (aref a 1) (aref a 4) (aref a 7)
              (aref a 2) (aref a 5) (aref a 8))))

(defun m3-invert (m tolerance)
  "Invert a 3x3 using cofactors.  This is about X times faster than
the Numerical Recipes code which uses Gaussian elimination."
  (let* ((inv (make-m3* (- (* (m3 m 1 1) (m3 m 2 2)) (* (m3 m 1 2) (m3 m 2 1)))
                        (- (* (m3 m 1 2) (m3 m 2 0)) (* (m3 m 1 0) (m3 m 2 2)))
                        (- (* (m3 m 1 0) (m3 m 2 1)) (* (m3 m 1 1) (m3 m 2 0)))
                        (- (* (m3 m 0 2) (m3 m 2 1)) (* (m3 m 0 1) (m3 m 2 2)))
                        (- (* (m3 m 0 0) (m3 m 2 2)) (* (m3 m 0 2) (m3 m 2 0)))
                        (- (* (m3 m 0 1) (m3 m 2 0)) (* (m3 m 0 0) (m3 m 2 1)))
                        (- (* (m3 m 0 1) (m3 m 1 2)) (* (m3 m 0 2) (m3 m 1 1)))
                        (- (* (m3 m 0 2) (m3 m 1 0)) (* (m3 m 0 0) (m3 m 1 2)))
                        (- (* (m3 m 0 0) (m3 m 1 1)) (* (m3 m 0 1) (m3 m 1 0)))))
         (det (+ (* (m3 m 0 0) (m3 inv 0 0))
                 (* (m3 m 0 1) (m3 inv 1 0))
                 (* (m3 m 0 2) (m3 inv 2 0)))))
    (when (> (abs det) tolerance)
      (let ((invdet (/ 1 det)))
        (map-into (m3-a inv) (lambda (x) (* x invdet)) (m3-a inv))
        inv))))

(defun m3-determinant (m)
  (let ((cofactor00 (- (* (m3 m 1 1) (m3 m 2 2)) (* (m3 m 1 2) (m3 m 2 1))))
        (cofactor10 (- (* (m3 m 1 2) (m3 m 2 0)) (* (m3 m 1 0) (m3 m 2 2))))
        (cofactor20 (- (* (m3 m 1 0) (m3 m 2 1)) (* (m3 m 1 1) (m3 m 2 0)))))
    (+ (* cofactor00 (m3 m 0 0))
       (* cofactor10 (m3 m 0 1))
       (* cofactor20 (m3 m 0 2)))))

(defun m3-bidiagonalise (ka kl kr)
  (let (identity)
    ;; map first column to (* 0 0)
    (let ((length (sqrt (+ (* (m3 ka 0 0) (m3 ka 0 0)) (* (m3 ka 1 0) (m3 ka 1 0)) (* (m3 ka 2 0) (m3 ka 2 0))))))
      (cond ((> length 0.0)
             (let* ((sign (if (> (m3 ka 0 0) 1.0) 1.0 -1.0))
                    (t1 (+ (m3 ka 0 0) (* sign length)))
                    (invt1 (/ 1 t1))
                    (v1 (* (m3 ka 1 0) invt1))
                    (v2 (* (m3 ka 2 0) invt1))
                    (t2 (/ -2.0 (+ 1.0 (* v1 v1) (* v2 v2))))
                    (w0 (* t2 (+ (m3 ka 0 0) (* (m3 ka 1 0) v1) (* (m3 ka 2 0) v2))))
                    (w1 (* t2 (+ (m3 ka 0 1) (* (m3 ka 1 1) v1) (* (m3 ka 2 1) v2))))
                    (w2 (* t2 (+ (m3 ka 0 2) (* (m3 ka 1 2) v1) (* (m3 ka 2 2) v2)))))
               (incf (m3 ka 0 0) w0)
               (incf (m3 ka 0 1) w1)
               (incf (m3 ka 0 2) w2)
               (incf (m3 ka 1 1) (* v1 w1))
               (incf (m3 ka 1 2) (* v1 w2))
               (incf (m3 ka 2 1) (* v2 w1))
               (incf (m3 ka 2 2) (* v2 w2))

               (setf (m3 kl 0 0) (1+ t2))
               (setf (m3 kl 0 1) (* t2 v1) (m3 kl 1 0) (* t2 v1))
               (setf (m3 kl 0 2) (* t2 v2) (m3 kl 2 0) (* t2 v2))
               (setf (m3 kl 1 1) (1+ (* t2 v1 v1)))
               (setf (m3 kl 1 2) (* t2 v1 v2) (m3 kl 2 1) (* t2 v1 v2))
               (setf (m3 kl 2 2) (1+ (* t2 v1 v2)))))
            (t
             (setv kl *m3-identity*)
             (setf identity t))))

    (flet ((compute-abc (t2 v2 &aux (b (* t2 v2)))
             (values (1+ t2) b (1+ (* b v2)))))
      ;; map first row to (* * 0)
      (let ((length (sqrt (+ (* (m3 ka 0 1) (m3 ka 0 1)) (* (m3 ka 0 2) (m3 ka 0 2))))))
        (cond ((> length 0.0)
               (let* ((sign (if (> (m3 ka 0 1) 1.0) 1.0 -1.0))
                      (t1 (+ (m3 ka 0 1) (* sign length)))
                      (v2 (/ (m3 ka 0 2) t1))
                      (t2 (/ -2.0 (+ 1.0 (* v2 v2))))
                      (w0 (* t2 (+ (m3 ka 0 1) (* (m3 ka 0 2) v2))))
                      (w1 (* t2 (+ (m3 ka 1 1) (* (m3 ka 1 2) v2))))
                      (w2 (* t2 (+ (m3 ka 2 1) (* (m3 ka 2 2) v2)))))
                 (incf (m3 ka 0 1) w0)
                 (incf (m3 ka 1 1) w1)
                 (incf (m3 ka 1 2) (* w1 v2))
                 (incf (m3 ka 2 1) w2)
                 (incf (m3 ka 2 2) (* w2 v2))
                 (multiple-value-bind (a b c) (compute-abc t2 v2)
                   (setf (m3 kr 0 0) 1.0
                         (m3 kr 0 1) 0.0 (m3 kr 1 0) 0.0
                         (m3 kr 0 2) 0.0 (m3 kr 2 0) 0.0
                         (m3 kr 1 1) a
                         (m3 kr 1 2) b (m3 kr 2 1) b
                         (m3 kr 2 2) c))))
              (t
               (setf identity t))))
      ;; map second column to (* * 0)
      (let ((length (sqrt (+ (* (m3 ka 1 1) (m3 ka 1 1)) (* (m3 ka 2 1) (m3 ka 2 1))))))
        (when (> length 0.0)
          (let* ((sign (if (> (m3 ka 1 1) 1.0) 1.0 -1.0))
                 (t1 (+ (m3 ka 1 1) (* sign length)))
                 (v2 (/ (m3 ka 2 1) t1))
                 (t2 (/ -2.0 (+ 1.0 (* v2 v2))))
                 (w1 (* t2 (+ (m3 ka 1 1) (* (m3 ka 2 1) v2))))
                 (w2 (* t2 (+ (m3 ka 1 2) (* (m3 ka 2 2) v2)))))
            (incf (m3 ka 1 1) w1)
            (incf (m3 ka 1 2) w2)
            (incf (m3 ka 2 2) (* v2 w2))
            (multiple-value-bind (a b c) (compute-abc t2 v2)
              (cond (identity
                     (setf (m3 kl 0 0) 1.0
                           (m3 kl 0 1) 0.0 (m3 kl 1 0) 0.0
                           (m3 kl 0 2) 0.0 (m3 kl 2 0) 0.0
                           (m3 kl 1 1) a
                           (m3 kl 1 2) b (m3 kl 2 1) b
                           (m3 kl 2 2) c))
                    (t
                     (dotimes (row 3)
                       (let ((tmp0 (m3 kl row 1))
                             (tmp1 (m3 kl row 2)))
                         (setf (m3 kl row 1) (+ (* a tmp0) (* b tmp1))
                               (m3 kl row 2) (+ (* b tmp0) (* c tmp1))))))))))))))

(defun m3<-axes (a0 a1 a2)
  (make-m3* (v3-x a0) (v3-y a0) (v3-z a0)
            (v3-x a1) (v3-y a1) (v3-z a1)
            (v3-x a2) (v3-y a2) (v3-z a2)))

(defstruct (matrix4 (:conc-name m4-) (:constructor make-m4 (&optional a)))
  "Storage format is concatenated set of columns:
c0x c0y c0z c0w c1x c1y c1z c1w c2x c2y c2z c2w c3x c3y c3z c3w."
  (a (make-array 16 :element-type 'real) :type (simple-array real (16))))

(defun copy-m4 (m)
  (make-m4 (copy-array (the (simple-array real (16)) (m4-a m)))))

(defun make-m4* (m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33)
  (make-m4 (make-array 16 :element-type 'real :initial-contents (vector m00 m10 m20 m30 m01 m11 m21 m31 m02 m12 m22 m32 m03 m13 m23 m33))))

(defvar *m4-zero* (make-m4* 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))
(defvar *m4-identity* (make-m4* 1.0 0.0 0.0 0.0 #||# 0.0 1.0 0.0 0.0 #||# 0.0 0.0 1.0 0.0 #||# 0.0 0.0 0.0 1.0))
(defvar *m4-clipspace-2d-to-imagespace* (make-m4* 0.5 0.0 0.0 0.0 #||# 0.0 -0.5 0.0 0.0 #||# 0.0 0.0 1.0 0.0 #||# 0.5 0.5 0.0 1.0)
  "Useful little matrix which takes 2D clipspace {-1,1} to {0,1} and inverts the Y.")

(defun m4 (m r c)
  (aref (the (simple-array real (16)) (m4-a m)) (+ r (ash c 2))))

(defun (setf m4) (v m r c)
  (setf (aref (the (simple-array real (16)) (m4-a m)) (+ r (ash c 2))) v))

(defmethod equal-p ((m1 matrix4) (m2 matrix4))
  (equalp (the (simple-array real (16)) (m4-a m1))
          (the (simple-array real (16)) (m4-a m2))))

(defmethod setv ((dest matrix4) (src matrix4))
  (map-into (m4-a dest) #'identity (m4-a src)))

(defmethod setv ((dest matrix4) (src matrix3))
  (let ((a4 (m4-a dest))
        (a3 (m3-a src)))
    (setf (aref a4 0) (aref a3 0)
          (aref a4 1) (aref a3 1)
          (aref a4 2) (aref a3 2)
          (aref a4 4) (aref a3 3)
          (aref a4 5) (aref a3 4)
          (aref a4 6) (aref a3 5)
          (aref a4 8) (aref a3 6)
          (aref a4 9) (aref a3 7)
          (aref a4 10) (aref a3 8))))

(defmethod setv ((dest matrix3) (src matrix4))
  (let ((a3 (m3-a dest))
        (a4 (m4-a src)))
    (setf (aref a3 0) (aref a4 0)
          (aref a3 1) (aref a4 1)
          (aref a3 2) (aref a4 2)
          (aref a3 3) (aref a4 4)
          (aref a3 4) (aref a4 5)
          (aref a3 5) (aref a4 6)
          (aref a3 6) (aref a4 8)
          (aref a3 7) (aref a4 9)
          (aref a3 8) (aref a4 10))))

(defun m4-minor (m r0 r1 r2 c0 c1 c2)
  (- (* (m4 m r0 c0) (- (* (m4 m r1 c1) (m4 m r2 c2)) (* (m4 m r2 c1) (m4 m r1 c2))))
     (* (m4 m r0 c1) (- (* (m4 m r1 c0) (m4 m r2 c2)) (* (m4 m r2 c0) (m4 m r1 c2))))
     (- (* (m4 m r0 c2) (- (* (m4 m r1 c0) (m4 m r2 c1)) (* (m4 m r2 c0) (m4 m r1 c1)))))))

(defun m4-trans (m)
  (let ((a (m4-a m)))
    (make-v3 (aref a 12) (aref a 13) (aref a 14))))

(defun (setf m4-trans) (v m)
  (let ((a (m4-a m)))
    (setf (aref a 12) (v3-x v)
          (aref a 13) (v3-y v)
          (aref a 14) (v3-z v))))

(defun m4-make-trans (v)
  (make-m4* 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 (v3-x v) (v3-y v) (v3-z v) 1.0))

(defun m4-make-trans* (x y z)
  (make-m4* 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0 x y z 1.0))

(defun m4-scale (m)
  (let ((a (m4-a m)))
    (make-v3 (aref a 0) (aref a 5) (aref a 10))))

(defun (setf m4-scale) (v m)
  (let ((a (m4-a m)))
    (setf (aref a 0) (v3-x v)
          (aref a 5) (v3-y v)
          (aref a 10) (v3-z v))))

(defun m4-make-scale (v)
  (make-m4* (v3-x v) 0.0 0.0 0.0 0.0 (v3-y v) 0.0 0.0 0.0 0.0 (v3-z v) 0.0 0.0 0.0 0.0 1.0))

(defun m4-make-scale* (x y z)
  (make-m4* x 0.0 0.0 0.0 0.0 y 0.0 0.0 0.0 0.0 z 0.0 0.0 0.0 0.0 1.0))

(defun m4-projection (m)
  (let ((a (m4-a m)))
    (make-v4 (aref a 3) (aref a 7) (aref a 11) (aref a 15))))

(defun (setf m4-projection) (v m)
  (let ((a (m4-a m)))
    (setf (aref a 3) (v4-x v)
          (aref a 7) (v4-y v)
          (aref a 11) (v4-z v)
          (aref a 15) (v4-w v))))

(defun m4+ (m1 m2)
  (make-m4 (map '(simple-array real (9)) #'+ (m4-a m1) (m4-a m2))))

(defun m4- (m1 m2)
  (make-m4 (map '(simple-array real (9)) #'- (m4-a m1) (m4-a m2))))

(defun m4-transpose (m)
  (let ((a (m4-a m)))
    (make-m4* (aref a 0) (aref a 4) (aref a 8) (aref a 12)
              (aref a 1) (aref a 5) (aref a 9) (aref a 13)
              (aref a 2) (aref a 6) (aref a 10) (aref a 14)
              (aref a 3) (aref a 7) (aref a 11) (aref a 15))))

(defun m4-concatenate (m1 m2)
  (make-m4* (+ (* (m4 m1 0 0) (m4 m2 0 0)) (* (m4 m1 0 1) (m4 m2 1 0)) (* (m4 m1 0 2) (m4 m2 2 0)) (* (m4 m1 0 3) (m4 m2 3 0)))
            (+ (* (m4 m1 1 0) (m4 m2 0 0)) (* (m4 m1 1 1) (m4 m2 1 0)) (* (m4 m1 1 2) (m4 m2 2 0)) (* (m4 m1 1 3) (m4 m2 3 0)))
            (+ (* (m4 m1 2 0) (m4 m2 0 0)) (* (m4 m1 2 1) (m4 m2 1 0)) (* (m4 m1 2 2) (m4 m2 2 0)) (* (m4 m1 2 3) (m4 m2 3 0)))
            (+ (* (m4 m1 3 0) (m4 m2 0 0)) (* (m4 m1 3 1) (m4 m2 1 0)) (* (m4 m1 3 2) (m4 m2 2 0)) (* (m4 m1 3 3) (m4 m2 3 0)))
                                                                                                                                  
            (+ (* (m4 m1 0 0) (m4 m2 0 1)) (* (m4 m1 0 1) (m4 m2 1 1)) (* (m4 m1 0 2) (m4 m2 2 1)) (* (m4 m1 0 3) (m4 m2 3 1)))
            (+ (* (m4 m1 1 0) (m4 m2 0 1)) (* (m4 m1 1 1) (m4 m2 1 1)) (* (m4 m1 1 2) (m4 m2 2 1)) (* (m4 m1 1 3) (m4 m2 3 1)))
            (+ (* (m4 m1 2 0) (m4 m2 0 1)) (* (m4 m1 2 1) (m4 m2 1 1)) (* (m4 m1 2 2) (m4 m2 2 1)) (* (m4 m1 2 3) (m4 m2 3 1)))
            (+ (* (m4 m1 3 0) (m4 m2 0 1)) (* (m4 m1 3 1) (m4 m2 1 1)) (* (m4 m1 3 2) (m4 m2 2 1)) (* (m4 m1 3 3) (m4 m2 3 1)))
                                                                                                                                  
            (+ (* (m4 m1 0 0) (m4 m2 0 2)) (* (m4 m1 0 1) (m4 m2 1 2)) (* (m4 m1 0 2) (m4 m2 2 2)) (* (m4 m1 0 3) (m4 m2 3 2)))
            (+ (* (m4 m1 1 0) (m4 m2 0 2)) (* (m4 m1 1 1) (m4 m2 1 2)) (* (m4 m1 1 2) (m4 m2 2 2)) (* (m4 m1 1 3) (m4 m2 3 2)))
            (+ (* (m4 m1 2 0) (m4 m2 0 2)) (* (m4 m1 2 1) (m4 m2 1 2)) (* (m4 m1 2 2) (m4 m2 2 2)) (* (m4 m1 2 3) (m4 m2 3 2)))
            (+ (* (m4 m1 3 0) (m4 m2 0 2)) (* (m4 m1 3 1) (m4 m2 1 2)) (* (m4 m1 3 2) (m4 m2 2 2)) (* (m4 m1 3 3) (m4 m2 3 2)))
                                                                                                                                  
            (+ (* (m4 m1 0 0) (m4 m2 0 3)) (* (m4 m1 0 1) (m4 m2 1 3)) (* (m4 m1 0 2) (m4 m2 2 3)) (* (m4 m1 0 3) (m4 m2 3 3)))
            (+ (* (m4 m1 1 0) (m4 m2 0 3)) (* (m4 m1 1 1) (m4 m2 1 3)) (* (m4 m1 1 2) (m4 m2 2 3)) (* (m4 m1 1 3) (m4 m2 3 3)))
            (+ (* (m4 m1 2 0) (m4 m2 0 3)) (* (m4 m1 2 1) (m4 m2 1 3)) (* (m4 m1 2 2) (m4 m2 2 3)) (* (m4 m1 2 3) (m4 m2 3 3)))
            (+ (* (m4 m1 3 0) (m4 m2 0 3)) (* (m4 m1 3 1) (m4 m2 1 3)) (* (m4 m1 3 2) (m4 m2 2 3)) (* (m4 m1 3 3) (m4 m2 3 3)))))

(defun m4-affine-p (m)
  (let ((a (m4-a m)))
    (and (zerop (aref a 3)) (zerop (aref a 7)) (zerop (aref a 11))
         (= 1 (aref a 15)))))

(defun m4-concatenate-affine (m1 m2)
  (assert (and (m4-affine-p m1) (m4-affine-p m2)))
  (make-m4* (+ (* (m4 m1 0 0) (m4 m2 0 0)) (* (m4 m1 0 1) (m4 m2 1 0)) (* (m4 m1 0 2) (m4 m2 2 0)))
            (+ (* (m4 m1 1 0) (m4 m2 0 0)) (* (m4 m1 1 1) (m4 m2 1 0)) (* (m4 m1 1 2) (m4 m2 2 0)))
            (+ (* (m4 m1 2 0) (m4 m2 0 0)) (* (m4 m1 2 1) (m4 m2 1 0)) (* (m4 m1 2 2) (m4 m2 2 0)))
            0.0
                                                                                                      
            (+ (* (m4 m1 0 0) (m4 m2 0 1)) (* (m4 m1 0 1) (m4 m2 1 1)) (* (m4 m1 0 2) (m4 m2 2 1)))
            (+ (* (m4 m1 1 0) (m4 m2 0 1)) (* (m4 m1 1 1) (m4 m2 1 1)) (* (m4 m1 1 2) (m4 m2 2 1)))
            (+ (* (m4 m1 2 0) (m4 m2 0 1)) (* (m4 m1 2 1) (m4 m2 1 1)) (* (m4 m1 2 2) (m4 m2 2 1)))
            0.0
                                                                                                      
            (+ (* (m4 m1 0 0) (m4 m2 0 2)) (* (m4 m1 0 1) (m4 m2 1 2)) (* (m4 m1 0 2) (m4 m2 2 2)))
            (+ (* (m4 m1 1 0) (m4 m2 0 2)) (* (m4 m1 1 1) (m4 m2 1 2)) (* (m4 m1 1 2) (m4 m2 2 2)))
            (+ (* (m4 m1 2 0) (m4 m2 0 2)) (* (m4 m1 2 1) (m4 m2 1 2)) (* (m4 m1 2 2) (m4 m2 2 2)))
            0.0
                                                                                                                                  
            (+ (* (m4 m1 0 0) (m4 m2 0 3)) (* (m4 m1 0 1) (m4 m2 1 3)) (* (m4 m1 0 2) (m4 m2 2 3)) (m4 m1 0 3))
            (+ (* (m4 m1 1 0) (m4 m2 0 3)) (* (m4 m1 1 1) (m4 m2 1 3)) (* (m4 m1 1 2) (m4 m2 2 3)) (m4 m1 1 3))
            (+ (* (m4 m1 2 0) (m4 m2 0 3)) (* (m4 m1 2 1) (m4 m2 1 3)) (* (m4 m1 2 2) (m4 m2 2 3)) (m4 m1 2 3))
            1.0))

(defmethod mult ((m1 matrix4) (m2 matrix4))
  (m4-concatenate m1 m2))

(defmethod mult ((m matrix4) (v vector3))
  "Vector transformation using '*'.
Transforms the given 3-D vector by the matrix, projecting the 
result back into w = 1.
This means that the initial w is considered to be 1.0,
and then all the tree elements of the resulting 3-D vector are
divided by the resulting w."
  (let ((invw (/ 1.0 (+ (* (m4 m 3 0) (v3-x v)) (* (m4 m 3 1) (v3-y v)) (* (m4 m 3 2) (v3-z v)) (m4 m 3 3)))))
    (make-v3 (* invw (+ (* (m4 m 0 0) (v3-x v)) (* (m4 m 0 1) (v3-y v)) (* (m4 m 0 2) (v3-z v)) (m4 m 0 3)))
             (* invw (+ (* (m4 m 1 0) (v3-x v)) (* (m4 m 1 1) (v3-y v)) (* (m4 m 1 2) (v3-z v)) (m4 m 1 3)))
             (* invw (+ (* (m4 m 2 0) (v3-x v)) (* (m4 m 2 1) (v3-y v)) (* (m4 m 2 2) (v3-z v)) (m4 m 2 3))))))

(defmethod mult ((m matrix4) (v vector4))
  (make-v4 (+ (* (m4 m 0 0) (v4-x v)) (* (m4 m 0 1) (v4-y v)) (* (m4 m 0 2) (v4-z v)) (* (m4 m 0 3) (v4-w v)))
           (+ (* (m4 m 1 0) (v4-x v)) (* (m4 m 1 1) (v4-y v)) (* (m4 m 1 2) (v4-z v)) (* (m4 m 1 3) (v4-w v)))
           (+ (* (m4 m 2 0) (v4-x v)) (* (m4 m 2 1) (v4-y v)) (* (m4 m 2 2) (v4-z v)) (* (m4 m 2 3) (v4-w v)))
           (+ (* (m4 m 3 0) (v4-x v)) (* (m4 m 3 1) (v4-y v)) (* (m4 m 3 2) (v4-z v)) (* (m4 m 3 3) (v4-w v)))))

(defmethod mult ((v vector4) (m matrix4))
  (make-v4 (+ (* (m4 m 0 0) (v4-x v)) (* (m4 m 1 0) (v4-y v)) (* (m4 m 2 0) (v4-z v)) (* (m4 m 3 0) (v4-w v)))
           (+ (* (m4 m 0 1) (v4-x v)) (* (m4 m 1 1) (v4-y v)) (* (m4 m 2 1) (v4-z v)) (* (m4 m 3 1) (v4-w v)))
           (+ (* (m4 m 0 2) (v4-x v)) (* (m4 m 1 2) (v4-y v)) (* (m4 m 2 2) (v4-z v)) (* (m4 m 3 2) (v4-w v)))
           (+ (* (m4 m 0 3) (v4-x v)) (* (m4 m 1 3) (v4-y v)) (* (m4 m 2 3) (v4-z v)) (* (m4 m 3 3) (v4-w v)))))

(defmethod mult-affine ((m matrix4) (v vector3))
  (assert (m4-affine-p m))
  (make-v3 (+ (* (m4 m 0 0) (v3-x v)) (* (m4 m 0 1) (v3-y v)) (* (m4 m 0 2) (v3-z v)))
           (+ (* (m4 m 1 0) (v3-x v)) (* (m4 m 1 1) (v3-y v)) (* (m4 m 1 2) (v3-z v)))
           (+ (* (m4 m 2 0) (v3-x v)) (* (m4 m 2 1) (v3-y v)) (* (m4 m 2 2) (v3-z v)))))

(defmethod mult-affine ((m matrix4) (v vector4))
  (assert (m4-affine-p m))
  (make-v4 (+ (* (m4 m 0 0) (v4-x v)) (* (m4 m 0 1) (v4-y v)) (* (m4 m 0 2) (v4-z v)) (* (m4 m 0 3) (v4-w v)))
           (+ (* (m4 m 1 0) (v4-x v)) (* (m4 m 1 1) (v4-y v)) (* (m4 m 1 2) (v4-z v)) (* (m4 m 1 3) (v4-w v)))
           (+ (* (m4 m 2 0) (v4-x v)) (* (m4 m 2 1) (v4-y v)) (* (m4 m 2 2) (v4-z v)) (* (m4 m 2 3) (v4-w v)))
           (v4-w v)))

(defun m4-adjoint (m)
  (make-m4* (m4-minor m 1 2 3 1 2 3)
            (- (m4-minor m 1 2 3 0 2 3))
            (m4-minor m 1 2 3 0 1 3)
            (- (m4-minor m 1 2 3 0 1 2))

            (- (m4-minor m 0 2 3 1 2 3))
            (m4-minor m 0 2 3 0 2 3)
            (- (m4-minor m 0 2 3 0 1 3))
            (m4-minor m 0 2 3 0 1 2)

            (m4-minor m 0 1 3 1 2 3)
            (- (m4-minor m 0 1 3 0 2 3))
            (m4-minor m 0 1 3 0 1 3)
            (- (m4-minor m 0 1 2 0 1 2))

            (- (m4-minor m 0 1 2 1 2 3))
            (m4-minor m 0 1 2 0 2 3)
            (- (m4-minor m 0 1 2 0 1 3))
            (m4-minor m 0 1 2 0 1 2)))

(defun m4-determinant (m)
  (- (+ (* (m4 m 0 0) (m4-minor m 1 2 3 1 2 3))
        (* (m4 m 0 2) (m4-minor m 1 2 3 0 1 3)))
     (* (m4 m 0 1) (m4-minor m 1 2 3 0 2 3))
     (* (m4 m 0 3) (m4-minor m 1 2 3 0 1 2))))

(defun m4-invert (m)
  (let ((m00 (m4 m 0 0)) (m01 (m4 m 0 1)) (m02 (m4 m 0 2)) (m03 (m4 m 0 3))
        (m10 (m4 m 1 0)) (m11 (m4 m 1 1)) (m12 (m4 m 1 2)) (m13 (m4 m 1 3))
        (m20 (m4 m 2 0)) (m21 (m4 m 2 1)) (m22 (m4 m 2 2)) (m23 (m4 m 2 3))
        (m30 (m4 m 3 0)) (m31 (m4 m 3 1)) (m32 (m4 m 3 2)) (m33 (m4 m 3 3)))
    (let ((v0 (- (* m20 m31) (* m21 m30)))
          (v1 (- (* m20 m32) (* m22 m30)))
          (v2 (- (* m20 m33) (* m23 m30)))
          (v3 (- (* m21 m32) (* m22 m31)))
          (v4 (- (* m21 m33) (* m23 m31)))
          (v5 (- (* m22 m33) (* m23 m32))))
      (let ((t00 (+ (+ (- (* v5 m11) (* v4 m12)) (* v3 m13))))
            (t10 (- (+ (- (* v5 m10) (* v2 m12)) (* v1 m13))))
            (t20 (+ (+ (- (* v4 m10) (* v2 m11)) (* v0 m13))))
            (t30 (- (+ (- (* v3 m10) (* v1 m11)) (* v0 m12)))))
        (let ((invdet (/ 1 (+ (* t00 m00) (* t10 m01) (* t20 m02) (* t30 m03)))))
          (let ((d00 (* t00 invdet))
                (d10 (* t10 invdet))
                (d20 (* t20 invdet))
                (d30 (* t30 invdet))
                (d01 (* (- (+ (- (* v5 m01) (* v4 m02)) (* v3 m03))) invdet))
                (d11 (* (+ (+ (- (* v5 m00) (* v2 m02)) (* v1 m03))) invdet))
                (d21 (* (- (+ (- (* v4 m00) (* v2 m01)) (* v0 m03))) invdet))
                (d31 (* (+ (+ (- (* v3 m00) (* v1 m01)) (* v0 m02))) invdet)))
            (let ((v0 (- (* m10 m31) (* m11 m30)))
                  (v1 (- (* m10 m32) (* m12 m30)))
                  (v2 (- (* m10 m33) (* m13 m30)))
                  (v3 (- (* m11 m32) (* m12 m31)))
                  (v4 (- (* m11 m33) (* m13 m31)))
                  (v5 (- (* m12 m33) (* m23 m32))))
              (let ((d02 (* (+ (+ (- (* v5 m01) (* v4 m02)) (* v3 m03))) invdet))
                    (d12 (* (- (+ (- (* v5 m00) (* v2 m02)) (* v1 m03))) invdet))
                    (d22 (* (+ (+ (- (* v4 m00) (* v2 m01)) (* v0 m03))) invdet))
                    (d32 (* (- (+ (- (* v3 m00) (* v1 m01)) (* v0 m02))) invdet)))
                (let ((v0 (- (* m10 m21) (* m11 m20)))
                      (v1 (- (* m10 m22) (* m12 m20)))
                      (v2 (- (* m10 m23) (* m13 m20)))
                      (v3 (- (* m11 m22) (* m12 m21)))
                      (v4 (- (* m11 m23) (* m13 m21)))
                      (v5 (- (* m12 m23) (* m23 m22))))
                  (let ((d03 (* (- (+ (- (* v5 m01) (* v4 m02)) (* v3 m03))) invdet))
                        (d13 (* (+ (+ (- (* v5 m00) (* v2 m02)) (* v1 m03))) invdet))
                        (d23 (* (- (+ (- (* v4 m00) (* v2 m01)) (* v0 m03))) invdet))
                        (d33 (* (+ (+ (- (* v3 m00) (* v1 m01)) (* v0 m02))) invdet)))
                    (make-m4* d00 d10 d20 d30
                              d01 d11 d21 d31
                              d02 d12 d22 d32
                              d03 d13 d23 d33)))))))))))

(defun m4-invert-affine (m)
  (assert (m4-affine-p m))
  (let ((m00 (m4 m 0 0)) (m01 (m4 m 0 1)) (m02 (m4 m 0 2)) (m03 (m4 m 0 3))
        (m10 (m4 m 1 0)) (m11 (m4 m 1 1)) (m12 (m4 m 1 2)) (m13 (m4 m 1 3))
        (m20 (m4 m 2 0)) (m21 (m4 m 2 1)) (m22 (m4 m 2 2)) (m23 (m4 m 2 3)))
    (let ((t00 (- (* m22 m11) (* m21 m12)))
          (t10 (- (* m20 m12) (* m22 m10)))
          (t20 (- (* m21 m10) (* m20 m11))))
      (let ((invdet (/ 1 (+ (* m00 t00) (* m01 t10) (* m02 t20)))))
        (let ((t00 (* t00 invdet)) (t10 (* t10 invdet)) (t20 (* t20 invdet))
              (m00 (* m00 invdet)) (m01 (* m01 invdet)) (m02 (* m02 invdet)))
          (let ((r00 t00)
                (r01 (- (* m02 m21) (* m01 m22)))
                (r02 (- (* m01 m12) (* m02 m11)))
                (r10 t10)
                (r11 (- (* m00 m22) (* m02 m20)))
                (r12 (- (* m02 m10) (* m00 m12)))
                (r20 t20)
                (r21 (- (* m01 m20) (* m00 m21)))
                (r22 (- (* m00 m11) (* m01 m10))))
            (let ((r03 (- (+ (* r00 m03) (* r01 m13) (* r02 m23))))
                  (r13 (- (+ (* r10 m03) (* r11 m13) (* r12 m23))))
                  (r23 (- (+ (* r20 m03) (* r21 m13) (* r22 m23)))))
              (make-m4* r00 r10 r20 0.0
                        r01 r11 r21 0.0
                        r02 r12 r22 0.0
                        r03 r13 r23 1.0))))))))

(defmethod mult ((m matrix4) (p plane))
  (let* ((n (plane-normal p))
         (v (mult (m4-transpose (m4-invert m))
                  (make-v4 (v3-x n) (v3-y n) (v3-z n) (plane-d p))))
         (new-normal (make-v3 (v4-x v) (v4-y v) (v4-z v))))
    (v3-normalisef new-normal)
    (error "~@<I think Ogre source has a bug in here: division of a scalar by a vector yields a vector, yet must yield a scalar, by this very local logic.~:@>")
    (make-plane new-normal (/ (v4-w v) new-normal))))

(defmethod mult ((m matrix4) (r real))
  (make-m4 (map '(simple-array real (16)) (lambda (x) (* x r)) (m4-a m))))

(defmethod mult ((r real) (m matrix4))
  (make-m4 (map '(simple-array real (16)) (lambda (x) (* x r)) (m4-a m))))

(defun m4-scalep (m)
  (not (and (real= (+ (* (m4 m 0 0) (m4 m 0 0)) (* (m4 m 1 0) (m4 m 1 0)) (* (m4 m 2 0) (m4 m 2 0))) 1.0 0.0001)
            (real= (+ (* (m4 m 0 1) (m4 m 0 1)) (* (m4 m 1 1) (m4 m 1 1)) (* (m4 m 2 1) (m4 m 2 1))) 1.0 0.0001)
            (real= (+ (* (m4 m 0 2) (m4 m 0 2)) (* (m4 m 1 2) (m4 m 1 2)) (* (m4 m 2 2) (m4 m 2 2))) 1.0 0.0001))))

(defun m4-negative-scalep (m)
  (minusp (m4-determinant m)))

(defun q<-m4 (m)
  (let ((m3 (make-m3)))
    (setv m3 m)
    (q<-m3 m3)))

(defun m4<-m3 (m)
  (lret ((m4 (make-m4)))
    (setv m4 *m4-identity*)
    (setv m4 m)))

(defun m4<-q (q)
  (lret ((m4 (make-m4)))
    (setv m4 *m4-identity*)
    (setv m4 (m3<-q q))))

(defun m4-make-transform (translation scale orientation)
  ;; Ordering:
  ;;    1. Scale
  ;;    2. Rotate
  ;;    3. Translate
  (lret ((m (m4<-m3 (mult (m3<-q orientation) (m3-make-scale scale)))))
    (setf (m4-trans m) translation
          (m4-projection m) *v4-unit-w*)))

(defun m4-make-inverse-transform (translation scale orientation)
  (let ((invtranslation (v3-neg translation))
        (invscale (make-v3 (/ 1 (v3-x scale)) (/ 1 (v3-y scale)) (/ 1 (v3-z scale))))
        (invorientation (q-invert orientation)))
    ;; Because we're inverting, order is translation, rotation, scale
    ;; So make translation relative to scale & rotation
    (let ((invtranslation (mult invorientation (mult invtranslation invscale))))
      (lret ((m (m4<-m3 (mult (m3-make-scale invscale) (m3<-q invorientation)))))
        (setf (m4-trans m) invtranslation
              (m4-projection m) *v4-unit-w*)))))
;;;;
;;;; Axis-aligned box
;;;;
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
(defstruct (axis-aligned-box (:conc-name aab-) (:constructor %make-axis-aligned-box (extent min max)))
  "A 3D box aligned with the x/y/z axes.

This class represents a simple box which is aligned with the
axes. Internally it only stores 2 points as the extremeties of
the box, one which is the minima of all 3 axes, and the other
which is the maxima of all 3 axes. This class is typically used
for an axis-aligned bounding box (AABB) for collision and
visibility determination."
  (extent nil :type (member :null :finite :infinite))
  (min nil :type vector3)
  (max nil :type vector3)
  (corners nil :type (or null simple-vector)))

(defvar *aab-null* (make-aab))
(defvar *aab-infinite* (make-aab :extent :infinite))

(defun aab-null-p (aab)       (eq (aab-extent aab) :null))
(defun aab-finite-p (aab)     (eq (aab-extent aab) :finite))
(defun aab-infinite-p (aab)   (eq (aab-extent aab) :infinite))
(defun aab-nullf (aab)        (setf (aab-extent aab) :null))
(defun aab-finitef (aab)      (setf (aab-extent aab) :finite))
(defun aab-infinitef (aab)    (setf (aab-extent aab) :infinite))
(defun aab-set-min (aab v3)
  (aab-finitef aab)
  (setf (aab-min aab) v3))
(defun aab-set-min* (aab x y z)
  (aab-finitef aab)
  (setf (v3-x (aab-min aab)) x
        (v3-y (aab-min aab)) y
        (v3-z (aab-min aab)) z))
(defun aab-set-max (aab v3)
  (aab-finitef aab)
  (setf (aab-max aab) v3))
(defun aab-set-max* (aab x y z)
  (aab-finitef aab)
  (setf (v3-x (aab-max aab)) x
        (v3-y (aab-max aab)) y
        (v3-z (aab-max aab)) z))
(defun aab-set-extents (aab min max)
  (aab-set-min aab min)
  (aab-set-max aab max))
(defun aab-set-extents* (aab min-x min-y min-z max-x max-y max-z)
  (aab-set-min* aab min-x min-y min-z)
  (aab-set-max* aab max-x max-y max-z))

(defun make-aab (&key extent origin)
  (multiple-value-bind (extent min max)
      (cond (extent (values extent (make-v3 -0.5 -0.5 -0.5) (make-v3 0.5 0.5 0.5)))
            (origin (values (aab-extent origin) (aab-min origin) (aab-max origin)))
            (t      (values :null (make-v3 -0.5 -0.5 -0.5) (make-v3 0.5 0.5 0.5))))
    (%make-axis-aligned-box extent min max)))

(defun make-aab* (min max)
  (%make-axis-aligned-box :finite min max))

(defun make-aab** (x-min y-min z-min x-max y-max z-max)
  (%make-axis-aligned-box :finite (make-v3 x-min y-min z-min) (make-v3 x-max y-max z-max)))

(defun aab-get-all-corners (aab)
  (unless (eq (aab-extent aab) :finite)
    (error "~@<Can't get corners of a null or infinite AAB.~:@>"))
  (setf (aab-corners aab)
        (make-array 8 :initial-contents
                    (list (aab-min aab)
                          (make-v3 (v3-x (aab-min aab)) (v3-y (aab-max aab)) (v3-z (aab-min aab)))
                          (make-v3 (v3-x (aab-max aab)) (v3-y (aab-max aab)) (v3-z (aab-min aab)))
                          (make-v3 (v3-x (aab-max aab)) (v3-y (aab-min aab)) (v3-z (aab-min aab)))
                          (aab-max aab)
                          (make-v3 (v3-x (aab-min aab)) (v3-y (aab-max aab)) (v3-z (aab-max aab)))
                          (make-v3 (v3-x (aab-min aab)) (v3-y (aab-min aab)) (v3-z (aab-max aab)))
                          (make-v3 (v3-x (aab-max aab)) (v3-y (aab-min aab)) (v3-z (aab-max aab)))))))

(defun aab-get-corner (aab corner)
  (case corner
    (:far-left-bottom   (aab-min aab))
    (:far-left-top      (make-v3 (v3-x (aab-min aab)) (v3-y (aab-max aab)) (v3-z (aab-min aab))))
    (:far-right-top     (make-v3 (v3-x (aab-max aab)) (v3-y (aab-max aab)) (v3-z (aab-min aab))))
    (:far-right-bottom  (make-v3 (v3-x (aab-max aab)) (v3-y (aab-min aab)) (v3-z (aab-min aab))))
    (:near-right-bottom (make-v3 (v3-x (aab-max aab)) (v3-y (aab-min aab)) (v3-z (aab-max aab))))
    (:near-left-bottom  (make-v3 (v3-x (aab-min aab)) (v3-y (aab-min aab)) (v3-z (aab-max aab))))
    (:near-left-top     (make-v3 (v3-x (aab-min aab)) (v3-y (aab-max aab)) (v3-z (aab-max aab))))
    (:near-right-top    (aab-max aab))))

(defun aab-mergef (aab1 aab2)
  "Merges the passed in box into the current box. The result is the
box which encompasses both."
  (cond ((or (aab-infinite-p aab1) (aab-null-p aab2)) aab1)
        ((aab-infinite-p aab2) (aab-infinitef aab1))
        ((aab-null-p aab1) (aab-set-extents aab1 (aab-min aab2) (aab-max aab2)))
        (t (aab-set-extents aab1 (v3-floor (aab-min aab1) (aab-min aab2)) (v3-floor (aab-max aab1) (aab-max aab2))))))

(defun aab-mergef* (aab v3)
  "Extends the box to encompass the specified point (if needed)."
  (ecase (aab-extent aab)
    (:null
     (aab-set-extents aab v3 v3))
    (:finite
     (v3-ceilf (aab-max aab) v3)
     (v3-floorf (aab-min aab) v3))
    (:infinite)))

(defun aab-transform (aab m4)
  "Transforms the box according to the matrix supplied.

By calling this method you get the axis-aligned box which
surrounds the transformed version of this box. Therefore each
corner of the box is transformed by the matrix, then the
extents are mapped back onto the axes to produce another
AABB. Useful when you have a local AABB for an object which
is then transformed."
  (when (aab-finite-p aab)
    (let ((oldmin (aab-min aab))
          (oldmax (aab-max aab))
          current-corner)
      (aab-nullf aab)
      ;; We sequentially compute the corners in the following order :
      ;; 0, 6, 5, 1, 2, 4 ,7 , 3
      ;; This sequence allows us to only change one member at a time to get at all corners.
      
      ;; For each one, we transform it using the matrix
      ;; Which gives the resulting point and merge the resulting point.
      (setf current-corner oldmin)
      (aab-mergef aab (mult m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmax))
      (aab-mergef aab (mult m4 current-corner))

      (setf (v3-y current-corner) (v3-y oldmax))
      (aab-mergef aab (mult m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmin))
      (aab-mergef aab (mult m4 current-corner))

      (setf (v3-x current-corner) (v3-x oldmax))
      (aab-mergef aab (mult m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmax))
      (aab-mergef aab (mult m4 current-corner))

      (setf (v3-y current-corner) (v3-y oldmin))
      (aab-mergef aab (mult m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmin))
      (aab-mergef aab (mult m4 current-corner)))))

(defun aab-transform-affine (aab m)
  "Transforms the box according to the affine matrix supplied.

By calling this method you get the axis-aligned box which
surrounds the transformed version of this box. Therefore each
corner of the box is transformed by the matrix, then the
extents are mapped back onto the axes to produce another
AABB. Useful when you have a local AABB for an object which
is then transformed.

The matrix must be an affine matrix. m4-affine-p."
  (assert (m4-affine-p m))
  (when (aab-finite-p aab)
    (let* ((centre (aab-centre aab))
           (halfsize (aab-halfsize aab))
           (new-centre (mult-affine m centre))
           (new-halfsize (make-v3 (+ (* (abs (m4 m 0 0)) (v3-x halfsize))
                                     (* (abs (m4 m 0 1)) (v3-y halfsize))
                                     (* (abs (m4 m 0 2)) (v3-z halfsize)))
                                  (+ (* (abs (m4 m 1 0)) (v3-x halfsize))
                                     (* (abs (m4 m 1 1)) (v3-y halfsize))
                                     (* (abs (m4 m 1 2)) (v3-z halfsize)))
                                  (+ (* (abs (m4 m 2 0)) (v3-x halfsize))
                                     (* (abs (m4 m 2 1)) (v3-y halfsize))
                                     (* (abs (m4 m 2 2)) (v3-z halfsize))))))
      (aab-set-extents aab (v3- new-centre new-halfsize) (v3+ new-centre new-halfsize)))))

(defun aab-intersects (aab1 aab2)
  (cond ((or (aab-null-p aab1) (aab-null-p aab2)) nil)
        ((or (aab-infinite-p aab1) (aab-infinite-p aab2)) t)
        ((or (< (v3-x (aab-max aab1)) (v3-x (aab-min aab2)))
             (< (v3-y (aab-max aab1)) (v3-y (aab-min aab2)))
             (< (v3-z (aab-max aab1)) (v3-z (aab-min aab2)))
             (> (v3-x (aab-min aab1)) (v3-x (aab-max aab2)))
             (> (v3-y (aab-min aab1)) (v3-y (aab-max aab2)))
             (> (v3-z (aab-min aab1)) (v3-z (aab-max aab2))))
         nil)
        (t t)))

(defun aab-intersection (aab1 aab2)
  (cond ((or (aab-null-p aab1) (aab-null-p aab2)) (make-aab))
        ((aab-infinite-p aab1) aab2)
        ((aab-infinite-p aab2) aab1)
        (t (let ((intmin (v3-ceil (aab-min aab1) (aab-min aab2)))
                 (intmax (v3-floor (aab-max aab1) (aab-max aab2))))
             (if (and (< (v3-x intmin) (v3-x intmax))
                      (< (v3-y intmin) (v3-y intmax))
                      (< (v3-z intmin) (v3-z intmax)))
                 (make-aab* intmin intmax)
                 (make-aab))))))

(defmethod volume ((aab axis-aligned-box))
  (ecase (aab-extent aab)
    (:null 0.0)
    (:finite (let ((d (v3- (aab-max aab) (aab-min aab))))
               (* (v3-x d) (v3-y d) (v3-z d))))
    (:infinite #+sbcl sb-ext:single-float-positive-infinity
               #-sbcl (error "~@<Don't know how to represent infinite values.~:@>"))))

(defmethod scale ((aab axis-aligned-box) (v3 vector3))
  (ecase (aab-extent aab)
    ((:null :infinite) aab)
    (:finite 
     (let ((min (copy-vector3 (aab-min aab)))
           (max (copy-vector3 (aab-max aab))))
       (v3-multf min v3)
       (v3-multf max v3)
       (make-aab* min max)))))

(defmethod scalef ((aab axis-aligned-box) (v3 vector3))
  (when (aab-finite-p aab)
    (v3-multf (aab-min aab) v3)
    (v3-multf (aab-max aab) v3)
    aab))

(defmethod intersects-p ((aab axis-aligned-box) (s sphere)) (ni))

(defmethod intersects-p ((aab axis-aligned-box) (p plane)) (ni))

(defmethod intersects-p ((aab axis-aligned-box) (v vector3))
  (ecase (aab-extent aab)
    (:null nil)
    (:infinite t)
    (:finite (and (>= (v3-x v) (v3-x (aab-min aab))) (<= (v3-x v) (v3-x (aab-max aab)))
                  (>= (v3-y v) (v3-y (aab-min aab))) (<= (v3-y v) (v3-y (aab-max aab)))
                  (>= (v3-z v) (v3-z (aab-min aab))) (<= (v3-z v) (v3-z (aab-max aab)))))))

(defun aab-centre (aab)
  (make-v3 (* (+ (v3-x (aab-min aab)) (v3-x (aab-max aab))) 0.5)
           (* (+ (v3-y (aab-min aab)) (v3-y (aab-max aab))) 0.5)
           (* (+ (v3-z (aab-min aab)) (v3-z (aab-max aab))) 0.5)))

(defun aab-size (aab)
  (ecase (aab-extent aab)
    (:null *v3-zero*)
    (:infinite #+sbcl (make-v3 sb-ext:single-float-positive-infinity
                               sb-ext:single-float-positive-infinity
                               sb-ext:single-float-positive-infinity)
               #-sbcl (error "~@<Don't know how to represent infinite values.~:@>"))
    (:finite (v3- (aab-max aab) (aab-min aab)))))

(defun aab-halfsize (aab)
  (ecase (aab-extent aab)
    (:null *v3-zero*)
    (:infinite #+sbcl (make-v3 sb-ext:single-float-positive-infinity
                               sb-ext:single-float-positive-infinity
                               sb-ext:single-float-positive-infinity)
               #-sbcl (error "~@<Don't know how to represent infinite values.~:@>"))
    (:finite (v3* (v3- (aab-max aab) (aab-min aab)) 0.5))))

(defmethod contains-p ((aab axis-aligned-box) (v vector3))
  (intersects-p aab v))

(defmethod contains-p ((aab axis-aligned-box) (containee axis-aligned-box))
  (cond ((or (aab-infinite-p aab) (aab-null-p containee)) t)
        ((or (aab-null-p aab) (aab-infinite-p containee)) nil)
        (t (and (>= (v3-x (aab-min containee)) (v3-x (aab-min aab))) (<= (v3-x (aab-max containee)) (v3-x (aab-max aab)))
                (>= (v3-y (aab-min containee)) (v3-y (aab-min aab))) (<= (v3-y (aab-max containee)) (v3-y (aab-max aab)))
                (>= (v3-z (aab-min containee)) (v3-z (aab-min aab))) (<= (v3-z (aab-max containee)) (v3-z (aab-max aab)))))))

(defmethod equal-p ((aab1 axis-aligned-box) (aab2 axis-aligned-box))
  (and (eq (aab-extent aab1) (aab-extent aab2))
       (or (not (aab-finite-p aab1))
           (and (v3= (aab-min aab1) (aab-min aab2))
                (v3= (aab-max aab1) (aab-max aab2))))))
