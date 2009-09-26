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

(defun +infinity ()
  #+sbcl sb-ext:single-float-positive-infinity
  #-sbcl (error "~@<Don't know how to represent infinite values.~:@>"))

(defun -infinity ()
  #+sbcl sb-ext:single-float-negative-infinity
  #-sbcl (error "~@<Don't know how to represent infinite values.~:@>"))

(defun real= (x1 x2 tolerance)
  (declare (type real x1 x2 tolerance))
  (< (abs (- x1 x2)) tolerance))

(defun iboundp (x a b)
  "Check whether X is within inclusive bounds [A, B]."
  (and (>= x a) (<= x b)))

(defun eboundp (x a b)
  "Check whether X is within exclusive bounds [A, B]."
  (and (> x a) (< x b)))

(defun rad<-deg (deg) (/ (* pi deg) 180.0))
(defun deg<-rad (rad) (/ (* 180.0 rad) pi))

(defun symmetric-random ()
  (- (random 2.0) 1.0))

(defgeneric volume (object))
(defgeneric scale (object v3))
(defgeneric scalef (object v3))
(defgeneric intersects-p (object-1 object-2 &key &allow-other-keys))
(defgeneric contains-p (object-1 object-2))
(defgeneric equal-p (object-1 object-2))
(defgeneric setv (object-1 object-2))
(defgeneric mult (object-1 object-2))
(defgeneric mult-affine (object-1 object-2))
(defgeneric dot (object-1 object-2))

(defun ni () (error "~@<This function is not implemented.~:@>"))

;;;;
;;;; Quaternions and vectors
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

(defstruct (quaternion (:conc-name q-) (:constructor make-q (w x y z)))
  (w 1.0 :type real)
  (x 0.0 :type real)
  (y 0.0 :type real)
  (z 0.0 :type real))

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

(defvar *q-zero* (make-q 0.0 0.0 0.0 0.0))
(defvar *q-identity* (make-q 1.0 0.0 0.0 0.0))

;;; quaternions
(defun q-x-axis (q)
  (let ((ty (* 2 (q-y q))) (tz (* 2 (q-z q))))
    (let ((wy (* ty (q-w q))) (wz (* tz (q-w q)))
          (xy (* ty (q-x q))) (xz (* tz (q-x q)))
          (yy (* ty (q-y q)))
          (zz (* tz (q-z q))))
      (make-v3 (- 1.0 yy zz) (+ xy wz) (- xz wy)))))

(defun q-y-axis (q)
  (let ((tx (* 2 (q-x q))) (ty (* 2 (q-y q))) (tz (* 2 (q-z q))))
    (let ((wx (* tx (q-w q))) (wz (* tz (q-w q)))
          (xx (* tx (q-x q))) (xy (* ty (q-x q)))
          (yz (* tz (q-y q)))
          (zz (* tz (q-z q))))
      (make-v3 (- xy wz) (- 1.0 xx zz) (+ yz wx)))))

(defun q-z-axis (q)
  (let ((tx (* 2 (q-x q))) (ty (* 2 (q-y q))) (tz (* 2 (q-z q))))
    (let ((wx (* tx (q-w q))) (wy (* ty (q-w q)))
          (xx (* tx (q-x q))) (xz (* tz (q-x q)))
          (yz (* tz (q-y q)))
          (zz (* tz (q-z q))))
      (make-v3 (+ xz wy) (- 1.0 xx zz) (+ yz wx)))))

(defun q+ (q1 q2)
  (make-q (+ (q-w q1) (q-w q2))
          (+ (q-x q1) (q-x q2))
          (+ (q-y q1) (q-y q2))
          (+ (q-z q1) (q-z q2))))

(defun q- (q1 q2)
  (make-q (- (q-w q1) (q-w q2))
          (- (q-x q1) (q-x q2))
          (- (q-y q1) (q-y q2))
          (- (q-z q1) (q-z q2))))

(defun q-neg (q)
  (make-q (- (q-w q)) (- (q-x q)) (- (q-y q)) (- (q-z q))))

(defmethod mult ((q1 quaternion) (q2 quaternion))
  (make-q (- (* (q-w q1) (q-w q2)) (* (q-x q1) (q-x q2)) (* (q-y q1) (q-y q2)) (* (q-z q1) (q-z q2)))
          (+ (* (q-w q1) (q-x q2)) (* (q-x q1) (q-w q2)) (* (q-y q1) (q-z q2)) (- (* (q-z q1) (q-y q2))))
          (+ (* (q-w q1) (q-y q2)) (* (q-y q1) (q-w q2)) (* (q-z q1) (q-x q2)) (- (* (q-x q1) (q-z q2))))
          (+ (* (q-w q1) (q-z q2)) (* (q-z q1) (q-w q2)) (* (q-x q1) (q-y q2)) (- (* (q-y q1) (q-x q2))))))

(defmethod mult ((q quaternion) (r real))
  (make-q (* (q-w q) r) (* (q-x q) r) (* (q-y q) r) (* (q-z q) r)))

(defmethod mult ((r real) (q quaternion))
  (make-q (* (q-w q) r) (* (q-x q) r) (* (q-y q) r) (* (q-z q) r)))

(defmethod dot ((q1 quaternion) (q2 quaternion))
  (+ (* (q-w q1) (q-w q2))
     (* (q-x q1) (q-x q2))
     (* (q-y q1) (q-y q2))
     (* (q-z q1) (q-z q2))))

(defun q-norm (q)
  (+ (* (q-w q) (q-w q))
     (* (q-x q) (q-x q))
     (* (q-y q) (q-y q))
     (* (q-z q) (q-z q))))

(defun q-invert (q)
  (let ((norm (q-norm q)))
    (if (> norm 0.0)
        (let ((inorm (/ 1 norm)))
          (make-q (* (q-w q) inorm)
                  (- (* (q-x q) inorm))
                  (- (* (q-y q) inorm))
                  (- (* (q-z q) inorm))))
        (error "~@<Attempted to invert a zero-norm quaternion.~:@>"))))

(defun q-unit-invert (q)
  (make-q (q-w q) (- (q-x q)) (- (q-y q)) (- (q-z q))))

(defun q-exp (q)
  "If q = A*(x*i+y*j+z*k) where (x,y,z) is unit length, then
exp(q) = cos(A)+sin(A)*(x*i+y*j+z*k).  If sin(A) is near zero,
use exp(q) = cos(A)+A*(x*i+y*j+z*k) since A/sin(A) has limit 1."
  (let* ((angle (sqrt (+ (* (q-x q) (q-x q)) (* (q-y q) (q-y q)) (* (q-z q) (q-z q)))))
         (sin (sin angle)))
    (multiple-value-bind (x y z)
        (if (> (abs sin) 0.0001)
            (let ((coeff (/ sin angle)))
              (values (* coeff (q-x q)) (* coeff (q-y q)) (* coeff (q-z q))))
            (values (q-x q) (q-y q) (q-z q)))
      (make-q (cos angle) x y z))))

(defun q-log (q)
  "If q = cos(A)+sin(A)*(x*i+y*j+z*k) where (x,y,z) is unit length, then
log(q) = A*(x*i+y*j+z*k).  If sin(A) is near zero, use log(q) =
sin(A)*(x*i+y*j+z*k) since sin(A)/A has limit 1."
  (let (angle sin)
    (multiple-value-bind (x y z)
        (if (or (< (abs (q-w q)) 1.0)
                (> (setf sin (sin (setf angle (acos (q-w q))))) 0.0001))
            (let ((coeff (/ angle sin)))
              (values (* coeff (q-x q)) (* coeff (q-y q)) (* coeff (q-z q))))
            (values (q-x q) (q-y q) (q-z q)))
      (make-q 0.0 x y z))))

(defun v3<-q (q)
  (make-v3 (q-x q) (q-y q) (q-z q)))

(defmethod mult ((q quaternion) (v vector3))
  "nVidia SDK implementation"
  (let* ((qvec (v3<-q q))
         (uv (v3-cross qvec v))
         (uuv (v3-cross qvec uv))
         (uv*2w (v3* uv (* 2.0 (q-w q))))
         (uuv*2 (v3* uuv 2.0)))
    (v3+ v (v3+ uv*2w uuv*2))))

(defun q-equal-p (q1 q2 tolerance)
  (let ((angle (acos (dot q1 q2))))
    (or (< angle tolerance)
        (real= angle pi tolerance))))

(defun q-slerp (ct p q &optional shortest-path)
  (let ((cos (dot q p)))
    ;; Do we need to invert rotation?
    (multiple-value-bind (cos tee) (if (and (< cos 0.0) shortest-path)
                                       (values (- cos) (q-neg q))
                                       (values cos q))
      (if (< (abs cos) -0.0001)
          ;; Standard case (slerp)
          (let* ((sin (sqrt (- 1.0 (* cos cos))))
                 (angle (atan sin cos))
                 (isin (/ 1.0 sin))
                 (coeff0 (* isin (sin (* (- 1.0 ct) angle))))
                 (coeff1 (* isin (sin (* ct angle)))))
            (q+ (mult coeff0 p) (mult coeff1 tee)))
          ;; There are two situations:
          ;; 1. "P" and "Q" are very close (fCos ~= +1), so we can do a linear
          ;;    interpolation safely.
          ;; 2. "P" and "Q" are almost inverse of each other (fCos ~= -1), there
          ;;    are an infinite number of possibilities interpolation. but we haven't
          ;;    have method to fix this case, so just use linear interpolation here.
          ;; Also, taking the complement requires renormalisation
          (q-normalise (q+ (mult (- 1.0 ct) p)
                           (mult ct tee)))))))

(defun q-slerp-extra-spins (ct p q extra-spins)
  (let ((angle (acos (dot p q))))
    (if (< (abs angle) 0.0001)
        p
        (let* ((isin (/ 1.0 (sin angle)))
               (phase (* pi extra-spins ct))
               (coeff0 (* isin (sin (- (* (- 1.0 ct) angle) phase))))
               (coeff1 (* isin (sin (+ (* ct angle) phase)))))
          (q+ (mult coeff0 p)
              (mult coeff1 q))))))

(defun q-intermediate (q0 q1 q2)
  "q0, q1, q2 are unit quaternions"
  (let ((iq0 (q-unit-invert q0))
        (iq1 (q-unit-invert q1)))
    (let ((p0 (mult iq0 q1))
          (p1 (mult iq1 q2)))
      (let ((arg (mult 0.25 (q- (q-log p0) (q-log p1)))))
        (values (mult q1 (q-exp arg))
                (mult q1 (q-exp (q-neg arg))))))))

(defun q-squad (ct p a b q shortest-path)
  (q-slerp (* 2.0 ct (- 1.0 ct))
           (q-slerp ct p q shortest-path)
           (q-slerp ct a b)))

(defun q-normalise (q)
  (let ((len (q-norm q)))
    (values (mult q (/ 1.0 (sqrt len)))
            len)))

(defun q-normalisef (q)
  (multiple-value-bind (newq len) (q-normalise q)
    (setf q newq)
    len))

(defun q-roll (q reproject-axis)
  "roll = atan2(localx.y, localx.x)"
  (let ((2xy+2wz (* 2.0 (+ (* (q-x q) (q-y q)) (* (q-w q) (q-z q))))))
    (if reproject-axis
        (atan 2xy+2wz (- 1.0 (* 2.0 (q-y q) (q-y q)) (* 2.0 (q-z q) (q-z q))))
        (atan 2xy+2wz (- (+ (* (q-w q) (q-w q)) (* (q-x q) (q-x q)))
                         (* (q-y q) (q-y q)) (* (q-z q) (q-z q)))))))

(defun q-pitch (q reproject-axis)
  "pitch = atan2(localy.z, localy.y)"
  (let ((2yz+2wx (* 2.0 (+ (* (q-y q) (q-z q)) (* (q-w q) (q-x q))))))
    (if reproject-axis
        (atan 2yz+2wx (- 1.0 (* 2.0 (q-x q) (q-x q)) (* 2.0 (q-z q) (q-z q))))
        (atan 2yz+2wx (- (+ (* (q-w q) (q-w q)) (* (q-z q) (q-z q)))
                         (* (q-x q) (q-x q)) (* (q-y q) (q-y q)))))))

(defun q-yaw (q reproject-axis)
  "pitch = atan2(localy.x, localy.z)"
  (let ((2xz+2wy (* 2.0 (+ (* (q-x q) (q-z q)) (* (q-w q) (q-y q))))))
    (if reproject-axis
        (atan 2xz+2wy (- 1.0 (* 2.0 (q-x q) (q-x q)) (* 2.0 (q-y q) (q-y q))))
        (asin (* 2.0 (- (* (q-w q) (q-y q)) (* (q-x q) (q-z q))))))))

(defun q-nlerp (ct p q shortest-path)
  (q-normalise (q+ p (mult ct (q- (if (and (< (dot p q) 0.0) shortest-path)
                                      (q-neg q)
                                      q)
                                  p)))))

;;; vectors
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
         (defun ,(name "V~D-ABS") (v)
           (,maker ,@(inst `(abs (,acc v)))))
         (defun ,(name "V~D-NEGF") (v)
           (setf ,@(minst `(,acc v) `(- (,acc v)))))
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
           (let ((length ( ,(name "V~D-LENGTH") v)))
             (when (> length epsilon)
               (let ((finv (/ 1 length)))
                 (setf ,@(minst `(,acc v) `(* (,acc v) finv))))
               v)))
         (defun ,(name "V~D-NORMALISE") (v)
           (,(name "V~D-NORMALISEF") (,(name "COPY-VECTOR~D") v)))
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
         (defun ,(name "V~D<=") (v1 v2)
           (and ,@(inst `(<= (,acc v1) (,acc v2)))))
         (defun ,(name "V~D>=") (v1 v2)
           (and ,@(inst `(>= (,acc v1) (,acc v2)))))
         (defmethod dot ((v1 ,(name "VECTOR~D")) (v2 ,(name "VECTOR~D")))
           (+ ,@(inst `(* (,acc v1) (,acc v2)))))
         (defun ,(name "V~D-ABS-DOT") (v1 v2)
           (+ ,@(inst `(abs (* (,acc v1) (,acc v2))))))
         (defun ,(name "V~D-REFLECT") (v normal)
           (,(name "V~D-") v (,(name "V~D*") (,(name "V~D*") (dot v normal) 2.0) normal)))
         (defun ,(name "V~D-ZEROP") (v)
           (< (,(name "V~D-LENGTH-SQUARED") v) epsilon-square))))))

(frob-vector-functions 2)
(frob-vector-functions 3)
(frob-vector-functions 4)

(defun v2-cross (v1 v2)
  (- (* (v2-x v1) (v2-y v2)) (* (v2-y v1) (v2-x v2))))

(defun v2-perpendicular (v)
  (make-v2 (- (v2-y v))
           (v2-x v)))

(defun v2-random-deviant (v angle)
  (let* ((angle (* 2 angle (random pi)))
         (sina (sin angle))
         (cosa (cos angle)))
    (make-v2 (- (* cosa (v2-x v)) (* sina (v2-y v)))
             (+ (* sina (v2-x v)) (* cosa (v2-y v))))))

(defun v2-in-tri-p (v a b c)
  (let (dot0 dot1 dot2 dot0-zerop dot1-zerop dot2-zerop)
    ;; Winding must be consistent from all edges for point to be inside
    (let ((v1 (v2- b a))
          (v2 (v2- v a)))
      ;; Note we don't care about normalisation here since sign is all we need
      ;; It means we don't have to worry about magnitude of cross products either
      (setf dot0 (v2-cross v1 v2)
            dot0-zerop (real= dot0 0.0 0.001)))
    (let ((v1 (v2- c b))
          (v2 (v2- v b)))
      (setf dot1 (v2-cross v1 v2)
            dot1-zerop (real= dot1 0.0 0.001)))
    ;; Compare signs (ignore colinear / coincident points)
    (if (and (not dot0-zerop) (not dot1-zerop)
             (not (= (signum dot0) (signum dot1))))
        nil
        (let ((v1 (v2- a c))
              (v2 (v2- v c)))
          (setf dot2 (v2-cross v1 v2)
                dot2-zerop (real= dot2 0.0 0.001))
          ;; Compare signs (ignore colinear / coincident points)
          (not (or (and (not dot0-zerop) (not dot2-zerop)
                        (not (= (signum dot0) (signum dot2))))
                   (and (not dot1-zerop) (not dot2-zerop)
                        (not (= (signum dot1) (signum dot2))))))))))

(defun v3-in-tri-p (v a b c normal)
  (let (dot0 dot1 dot2 dot0-zerop dot1-zerop dot2-zerop)
    ;; Winding must be consistent from all edges for point to be inside
    (let ((v1 (v2- b a))
          (v2 (v2- v a)))
      ;; Note we don't care about normalisation here since sign is all we need
      ;; It means we don't have to worry about magnitude of cross products either
      (setf dot0 (dot (v2-cross v1 v2) normal)
            dot0-zerop (real= dot0 0.0 0.001)))
    (let ((v1 (v2- c b))
          (v2 (v2- v b)))
      (setf dot1 (dot (v2-cross v1 v2) normal)
            dot1-zerop (real= dot1 0.0 0.001)))
    ;; Compare signs (ignore colinear / coincident points)
    (if (and (not dot0-zerop) (not dot1-zerop)
             (not (= (signum dot0) (signum dot1))))
        nil
        (let ((v1 (v2- a c))
              (v2 (v2- v c)))
          (setf dot2 (dot (v2-cross v1 v2) normal)
                dot2-zerop (real= dot2 0.0 0.001))
          ;; Compare signs (ignore colinear / coincident points)
          (not (or (and (not dot0-zerop) (not dot2-zerop)
                        (not (= (signum dot0) (signum dot2))))
                   (and (not dot1-zerop) (not dot2-zerop)
                        (not (= (signum dot1) (signum dot2))))))))))

(defun v3-cross (v1 v2)
  (make-v3 (- (* (v3-y v1) (v3-z v2)) (* (v3-z v1) (v3-y v2)))
           (- (* (v3-z v1) (v3-x v2)) (* (v3-x v1) (v3-z v2)))
           (- (* (v3-x v1) (v3-y v2)) (* (v3-y v1) (v3-x v2)))))

(defun v3-perpendicular (v)
  (let ((perp (v3-cross v *v3-unit-x*)))
    (lret ((perp (if (< (v3-length-squared perp) epsilon-square)
                     (v3-cross v *v3-unit-y*)
                     perp)))
      (v3-normalisef perp))))

(defun v3-random-deviant (v angle &optional (up (v3-perpendicular v)))
  ;; Rotate up vector by random amount around v
  (let* ((q1 (q<-angle-axis (* 2 pi (random 1.0)) v))
         (new-up (mult q1 up))
         ;; Finally rotate v by given angle around randomised up
         (q2 (q<-angle-axis angle new-up)))
    (mult q2 v)))

(defun v3-angle-between (v1 v2)
  (let ((lenproduct (max (* (v3-length v1) (v3-length v2)) epsilon-square)))
    (acos (clamp (/ (dot v1 v2) lenproduct) -1.0 1.0))))

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
         (ndot (dot v1n v2n)))
    (when (>= ndot 1.0)
      (return-from v3-rotation-to *q-identity*))
    (if (< ndot (- epsilon 1.0))
        (if fallback-axis
            (q<-angle-axis pi fallback-axis)
            (let ((axis (v3-cross *v3-unit-x* v1)))
              (let ((axis (if (v3-zerop axis) (v3-cross *v3-unit-y* v1))))
                (q<-angle-axis pi (v3-normalise axis)))))
        (let* ((s (sqrt (* 2 (1+ ndot))))
               (is (/ 1 s))
               (c (v3-cross v1n v2n)))
          (q-normalise (make-q (* (v3-x c) is)
                               (* (v3-y c) is)
                               (* (v3-z c) is)
                               (* s 0.5)))))))

(defun v3-position-equalp (v1 v2 &optional (tolerance 0.001))
  (and (real= (v3-x v1) (v3-x v2) tolerance)
       (real= (v3-y v1) (v3-y v2) tolerance)
       (real= (v3-z v1) (v3-z v2) tolerance)))

(defun v3-position-closesp (v1 v2 &optional (tolerance 0.001))
  (<= (v3-distance-squared v1 v2) 
      (* tolerance (+ (v3-length-squared v1) (v3-length-squared v2)))))

(defun v3-direction-equalp (v1 v2 tolerance)
  (let ((angle (acos (dot v1 v2))))
    (<= (abs angle) tolerance)))

(defmacro with-v3-accessor ((accessor v3) &body body)
  (with-gensyms (i val)
    (once-only (v3)
      `(flet ((,accessor (,i) (ecase ,i (0 (v3-x ,v3)) (1 (v3-y ,v3)) (2 (v3-z ,v3))))
              ((setf ,accessor) (,val ,i) (case ,i (0 (setf (v3-x ,v3) ,val)) (1 (setf (v3-y ,v3) ,val)) (2 (setf (v3-z ,v3) ,val)))))
         (declare (ignorable (function ,accessor) (function (setf ,accessor))))
         ,@body))))

(defun v3face-normal-cheap (v0 v1 v2)
  (v3-cross (v3- v1 v0) (v3- v2 v0)))

(defun v3face-normal (v0 v1 v2)
  (v3-normalizef (v3-cross (v3- v1 v0) (v3- v2 v0))))

;;;;
;;;; Trivial geometrics
;;;;
(defstruct (ray (:constructor make-ray (orig dir)))
  (orig (make-v3 0.0 0.0 0.0) :type vector3)
  (dir (make-v3 1.0 0.0 0.0) :type vector3))

(defstruct (sphere (:constructor make-sphere (orig rad)))
  (orig (make-v3 0.0 0.0 0.0) :type vector3)
  (rad 0 :type real))

(defstruct plane)
(defstruct (normal-plane (:include plane) (:conc-name plane-) (:constructor make-normal-plane (normal d)))
  (normal (make-v3 0.0 1.0 0.0) :type vector3)
  (d 0 :type real))
(defstruct (3vec-plane (:include plane) (:conc-name plane-) (:constructor make-3vec-plane (a b c)))
  (a (make-v3 0.0 0.0 0.0) :type vector3)
  (b (make-v3 1.0 0.0 0.0) :type vector3)
  (c (make-v3 0.0 0.0 1.0) :type vector3))
(defstruct (3vec-normal-plane (:include 3vec-plane) (:conc-name 3vec-plane-) (:constructor make-3vec-normal-plane (a b c normal)))
  (normal (make-v3 0.0 1.0 0.0) :type vector3))

(defstruct (planeset (:constructor make-planeset (set)))
  (set nil :type list))

(defun 3vec-plane-extend (p)
  (declare (type 3vec-plane p))
  (make-3vec-normal-plane (plane-a p) (plane-b p) (plane-c p)
                          (v3face-normal-cheap (plane-a p) (plane-b p) (plane-c p))))

(defun ray-point (r k)
  "Gets the position of a point k units along the ray."
  (v3+ (ray-orig r) (v3* (ray-dir r) k)))

(defmethod mult ((r ray) (k real))
  (ray-point r k))

(defmethod intersects-p ((r ray) (p normal-plane) &key &allow-other-keys)
  (let ((denom (dot (plane-normal p) (ray-dir r))))
    (if (< (abs denom) epsilon)
        ;; parallel
        (values nil 0)
        (let* ((nom (+ (dot (plane-normal p) (ray-orig r)) (plane-d p)))
               (tee (- (/ nom denom))))
          (values (plusp tee) t)))))

(defmethod intersects-p ((r ray) (pset planeset) &key normal-outside-p &allow-other-keys)
  ;; derive side
  (let ((all-inside t)
        (outside (if normal-outside-p :outside :inside))
        intersectp (distance 0.0)
        end-intersectp end-distance)
    (dolist (p (planeset-set pset))
      (multiple-value-bind (intersects dist) (intersects-p r p)
        ;; is origin outside?
        (if (eq outside (plane-side p (ray-orig r)))
            ;; Test single plane
            (progn
              (setf all-inside nil)
              (if intersects
                  (setf intersectp t
                        distance (max dist distance))
                  (return-from intersects-p (values nil 0.0))))
            (when intersects
              (if end-intersectp
                  (setf end-distance (min dist end-distance))
                  (setf end-intersectp t
                        end-distance distance))))))
    (cond (all-inside
           ;; Intersecting at 0 distance since inside the volume!
           (values t 0.0))
          ((and end-intersectp (< end-distance distance))
           nil)
          (t
           (values intersectp distance)))))

(defmethod intersects-p ((r ray) (s sphere) &key discard-inside-p &allow-other-keys)
  ;; Adjust ray origin relative to sphere center
  (let ((rayorig (v3- (ray-orig r) (sphere-orig s)))
        (rad (sphere-rad s)))
    ;; Check origin inside first
    (if (and discard-inside-p (<= (v3-length-squared rayorig) (* rad rad)))
        (values t 0.0)
        ;; Mmm, quadratics
        ;; Build coeffs which can be used with std quadratic solver
        ;; ie t = (-b +/- sqrt(b*b + 4ac)) / 2a
        (let ((a (dot (ray-dir r) (ray-dir r)))
              (b (* 2.0 (dot (ray-orig r) (ray-dir r))))
              (c (- (dot (ray-orig r) (ray-orig r)) (* rad rad))))
          ;; determinant
          (let ((det (- (* b b) (* 4.0 a c))))
            (if (minusp det)
                ;; no intersection
                nil
                ;; BTW, if d=0 there is one intersection, if d > 0 there are 2
                ;; But we only want the closest one, so that's ok, just use the 
                ;; '-' version of the solver
                (let ((tee (/ (- (- b) (sqrt det)) (* 2.0 a))))
                  (values t (if (minusp tee)
                                (/ (+ (- b) (sqrt det)) (* 2.0 a))
                                tee)))))))))

(defmethod intersects-p ((r ray) (p 3vec-plane) &key &allow-other-keys)
  (intersects-p r (3vec-plane-extend p)))

(defmethod intersects-p ((r ray) (p 3vec-normal-plane) &key positive-side negative-side &allow-other-keys)
  (let ((n (3vec-plane-normal p)))
    ;; Calculate intersection with plane.
    (let ((denom (dot n (ray-dir r))))
      ;; Check intersect side
      (cond ((> denom epsilon)
             (unless negative-side
               (return-from intersects-p (values nil 0.0))))
            ((< denom (- epsilon))
             (unless positive-side
               (return-from intersects-p (values nil 0.0))))
            (t
             ;; Parallel or triangle area is close to zero when
             ;; the plane normal not normalised.
             (return-from intersects-p (values nil 0.0))))
      (let ((tee (/ (dot n (v3- (plane-a p) (ray-orig r))) denom)))
        (if (minusp tee)
            (values nil 0.0)
            ;; Calculate the largest area projection plane in X, Y or Z.
            (let ((absn (v3-abs n)))
              (multiple-value-bind (i0 i1) (if (> (v3-y absn) (v3-z absn))
                                               (values (if (> (v3-y absn) (v3-x absn)) 0 1) 2)
                                               (values 1 (if (> (v3-z absn) (v3-x absn)) 0 2)))
                ;; Check the intersection point is inside the triangle.
                (with-v3-accessor (a (plane-a p))
                  (with-v3-accessor (b (plane-b p))
                    (with-v3-accessor (c (plane-c p))
                      (with-v3-accessor (orig (ray-orig r))
                        (with-v3-accessor (dir (ray-dir r))
                          (let ((u1 (- (b i0) (a i0)))
                                (v1 (- (b i1) (a i1)))
                                (u2 (- (c i0) (a i0)))
                                (v2 (- (c i1) (a i1)))
                                (u0 (+ (* tee (dir i0)) (orig i0) (- (a i0))))
                                (v0 (+ (* tee (dir i1)) (orig i1) (- (a i1)))))
                            (let ((alpha (- (* u0 v2) (* u2 v0)))
                                  (beta (- (* u1 v0) (* u0 v1)))
                                  (area (- (* u1 v2) (* u2 v1)))) ; epsilon to avoid float precision error
                              (let ((tolerance (* -0.000001 area)))
                                (if (plusp area)
                                    (if (or (< alpha tolerance) (< beta tolerance) (> (+ alpha beta) (- area tolerance)))
                                        (values nil 0.0)
                                        (values t tee))
                                    (if (or (> alpha tolerance) (> beta tolerance) (< (+ alpha beta) (- area tolerance)))
                                        (values nil 0.0)
                                        (values t tee))))))))))))))))))

;;;;
;;;; Matrices
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

(defun m3* (m r1 c1 r2 c2)
  (let ((a (m3-a m)))
    (* (aref a (+ r1 (* 3 c1)))
       (aref a (+ r2 (* 3 c2))))))

(defun m3*3 (m r1 c1 r2 c2 r3 c3)
  (let ((a (m3-a m)))
    (* (aref a (+ r1 (* 3 c1)))
       (aref a (+ r2 (* 3 c2)))
       (aref a (+ r3 (* 3 c3))))))

(defun m3** (m r c)
  (let ((ref (aref (m3-a m) (+ r (* 3 c)))))
    (* ref ref)))

(defmethod equal-p ((m1 matrix3) (m2 matrix3))
  (equalp (the (simple-array real (9)) (m3-a m1))
          (the (simple-array real (9)) (m3-a m2))))

(defmethod setv ((dest matrix3) (src matrix3))
  (map-into (m3-a dest) #'identity (m3-a src)))

(defmethod setv ((dest matrix3) (v vector))
  (map-into (m3-a dest) #'identity v))

(defun m3+ (m1 m2)
  (make-m3 (map '(simple-array real (9)) #'+ (m3-a m1) (m3-a m2))))

(defun m3- (m1 m2)
  (make-m3 (map '(simple-array real (9)) #'- (m3-a m1) (m3-a m2))))

(defun m3-column (m c)
  (let ((a (m3-a m))
        (coff (* 3 c)))
    (make-v3 (aref a (+ 0 coff)) (aref a (+ 1 coff)) (aref a (+ 2 coff)))))

(defun (setf m3-column) (v m c)
  (let ((a (m3-a m))
        (coff (* 3 c)))
    (setf (aref a (+ 0 coff)) (v3-x v)
          (aref a (+ 1 coff)) (v3-y v)
          (aref a (+ 2 coff)) (v3-z v))))

(defun m3-extract-column (m c v)
  (let ((a (m3-a m))
        (coff (* 3 c)))
    (setf (v3-x v) (aref a (+ 0 coff))
          (v3-y v) (aref a (+ 1 coff))
          (v3-z v) (aref a (+ 2 coff)))))

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
  (not (and (real= (+ (m3** m 0 0) (m3** m 1 0) (m3** m 2 0)) 1.0 0.0001)
            (real= (+ (m3** m 0 1) (m3** m 1 1) (m3** m 2 1)) 1.0 0.0001)
            (real= (+ (m3** m 0 2) (m3** m 1 2) (m3** m 2 2)) 1.0 0.0001))))

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
  (let* ((inv (make-m3* (- (m3* m 1 1 2 2) (m3* m 1 2 2 1))
                        (- (m3* m 1 2 2 0) (m3* m 1 0 2 2))
                        (- (m3* m 1 0 2 1) (m3* m 1 1 2 0))
                        (- (m3* m 0 2 2 1) (m3* m 0 1 2 2))
                        (- (m3* m 0 0 2 2) (m3* m 0 2 2 0))
                        (- (m3* m 0 1 2 0) (m3* m 0 0 2 1))
                        (- (m3* m 0 1 1 2) (m3* m 0 2 1 1))
                        (- (m3* m 0 2 1 0) (m3* m 0 0 1 2))
                        (- (m3* m 0 0 1 1) (m3* m 0 1 1 0))))
         (det (+ (* (m3 m 0 0) (m3 inv 0 0))
                 (* (m3 m 0 1) (m3 inv 1 0))
                 (* (m3 m 0 2) (m3 inv 2 0)))))
    (when (> (abs det) tolerance)
      (let ((idet (/ 1 det)))
        (map-into (m3-a inv) (lambda (x) (* x idet)) (m3-a inv))
        inv))))

(defun m3-determinant (m)
  (let ((cofactor00 (- (m3* m 1 1 2 2) (m3* m 1 2 2 1)))
        (cofactor10 (- (m3* m 1 2 2 0) (m3* m 1 0 2 2)))
        (cofactor20 (- (m3* m 1 0 2 1) (m3* m 1 1 2 0))))
    (+ (* cofactor00 (m3 m 0 0))
       (* cofactor10 (m3 m 0 1))
       (* cofactor20 (m3 m 0 2)))))

(defun m3-bidiagonalise (ka kl kr)
  (let (identity)
    ;; map first column to (* 0 0)
    (let ((length (sqrt (+ (m3** ka 0 0) (m3** ka 1 0) (m3** ka 2 0)))))
      (cond ((> length 0.0)
             (let* ((sign (if (> (m3 ka 0 0) 1.0) 1.0 -1.0))
                    (t1 (+ (m3 ka 0 0) (* sign length)))
                    (it1 (/ 1 t1))
                    (v1 (* (m3 ka 1 0) it1))
                    (v2 (* (m3 ka 2 0) it1))
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
      (let ((length (sqrt (+ (m3** ka 0 1) (m3** ka 0 2)))))
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
      (let ((length (sqrt (+ (m3** ka 1 1) (m3** ka 2 1)))))
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

;; These four are common to Golub-Kahan and SVD
(defun prod0 (sin cos tmp0 tmp1) (- (* cos tmp0) (* sin tmp1)))
(defun prod1 (sin cos tmp0 tmp1) (+ (* sin tmp0) (* cos tmp1)))

(defun m3-fudge-columnwise (m r0 r1 sin cos)
  (dotimes (col 3)
    (let ((tmp0 (m3 m r0 col))
          (tmp1 (m3 m r1 col)))
      (setf (m3 m r0 col) (prod0 sin cos tmp0 tmp1)
            (m3 m r1 col) (prod1 sin cos tmp0 tmp1)))))

(defun m3-fudge-rowwise (m c0 c1 sin cos)
  (dotimes (row 3)
    (let ((tmp0 (m3 m row c0))
          (tmp1 (m3 m row c1)))
      (setf (m3 m row c0) (prod0 sin cos tmp0 tmp1)
            (m3 m row c1) (prod1 sin cos tmp0 tmp1)))))

;;;
;;; Big fat note: row/column nomenclature was a bit fudged in the original source.
;;;
(defun m3-golub-kahan-step (a l r)
  (let ((t11 (+ (m3** a 0 1) (m3** a 1 1)))
        (t22 (+ (m3** a 1 2) (m3** a 2 2)))
        (t12 (m3* a 1 1 1 2)))
    (let ((trace (+ t11 t22))
          (diff (- t11 t22)))
      (let ((discr (sqrt (+ (* diff diff) (* 4.0 t12 t12)))))
        (let ((root1 (* 0.5 (+ trace discr)))
              (root2 (* 0.5 (- trace discr))))
          ;; adjust right
          (let ((y (- (m3 a 0 0) (if (<= (abs (- root1 t22)) (abs (- root2 t22))) root1 root2)))
                (z (m3 a 0 1)))
            (let ((ilength (/ 1.0 (sqrt (+ (* y y) (* z z))))))
              (let ((sin (* z ilength))
                    (cos (* -1.0 y ilength))
                    (tmp0 (m3 a 0 0))
                    (tmp1 (m3 a 0 1)))
                (setf (m3 a 0 0) (prod0 sin cos tmp0 tmp1)
                      (m3 a 0 1) (prod1 sin cos tmp0 tmp1)
                      (m3 a 1 0) (* -1.0 sin (m3 a 1 1))
                      (m3 a 1 1) (* cos (m3 a 1 1)))
                (m3-fudge-columnwise r 0 1 sin cos))))
          ;; adjust left
          (let ((y (m3 a 0 0))
                (z (m3 a 1 0)))
            (let ((ilength (/ 1.0 (sqrt (+ (* y y) (* z z))))))
              (let ((sin (* z ilength))
                    (cos (* -1.0 y ilength))
                    (tmp0 (m3 a 0 1))
                    (tmp1 (m3 a 1 1)))
                (setf (m3 a 0 0) (- (* cos (m3 a 0 0)) (* sin (m3 a 1 0)))
                      (m3 a 0 1) (prod0 sin cos tmp0 tmp1)
                      (m3 a 1 1) (prod1 sin cos tmp0 tmp1)
                      (m3 a 0 2) (* -1.0 sin (m3 a 1 2))
                      (m3 a 1 2) (* cos (m3 a 1 2)))
                (m3-fudge-rowwise l 0 1 sin cos))))
          ;; adjust right
          (let ((y (m3 a 0 1))
                (z (m3 a 0 2)))
            (let ((ilength (/ 1.0 (sqrt (+ (* y y) (* z z))))))
              (let ((sin (* z ilength))
                    (cos (* -1.0 y ilength))
                    (tmp0 (m3 a 1 1))
                    (tmp1 (m3 a 1 2)))
                (setf (m3 a 0 1) (- (* cos (m3 a 0 1)) (* sin (m3 a 0 2)))
                      (m3 a 1 1) (prod0 sin cos tmp0 tmp1)
                      (m3 a 1 2) (prod1 sin cos tmp0 tmp1)
                      (m3 a 2 1) (* -1.0 sin (m3 a 2 2))
                      (m3 a 2 2) (* cos (m3 a 2 2)))
                (m3-fudge-columnwise r 1 2 sin cos))))
          ;; adjust left
          (let ((y (m3 a 1 1))
                (z (m3 a 2 1)))
            (let ((ilength (/ 1.0 (sqrt (+ (* y y) (* z z))))))
              (let ((sin (* z ilength))
                    (cos (* -1.0 y ilength))
                    (tmp0 (m3 a 1 2))
                    (tmp1 (m3 a 2 2)))
                (setf (m3 a 1 1) (- (* cos (m3 a 1 1)) (* sin (m3 a 2 1)))
                      (m3 a 1 2) (prod0 sin cos tmp0 tmp1)
                      (m3 a 2 2) (prod1 sin cos tmp0 tmp1))
                (m3-fudge-rowwise l 1 2 sin cos)))))))))

;;; Same problem with row/col nomenclature botchage as with Golub-Kahan
(defun m3-singular-value-decomposition (m l s r &optional (max-iterations 32))
  (let ((a (copy-m3 m)))
    (m3-bidiagonalise a l r)
    (iter (repeat max-iterations)
          (let ((test1 (<= (abs (m3 a 0 1)) (+ (* 0.0001 (abs (m3 a 0 0))) (abs (m3 a 1 1)))))
                (test2 (<= (abs (m3 a 1 2)) (+ (* 0.0001 (abs (m3 a 1 1))) (abs (m3 a 2 2))))))
            (if test1
                (if test2
                    (progn
                      (setf (v3-x s) (m3 a 0 0)
                            (v3-y s) (m3 a 1 1)
                            (v3-z s) (m3 a 2 2))
                      (break))
                    ;; 2x2 closed form factorisation
                    (let* ((tmp (/ (+ (m3** a 1 1) (- (m3** a 2 2)) (m3** a 1 2))
                                   (m3* a 1 2 2 2)))
                           (tan0 (* 0.5 (+ tmp (sqrt (+ 4.0 (* tmp tmp))))))
                           (cos0 (/ 1.0 (sqrt (+ 1.0 (* tan0 tan0)))))
                           (sin0 (* tan0 cos0)))
                      (m3-fudge-rowwise l 1 2 sin0 cos0)
                      (let* ((tan1 (/ (- (m3 a 1 2) (* tan0 (m3 a 2 2))) (m3 a 1 1)))
                             (cos1 (/ 1.0 (sqrt (+ 1.0 (* tan1 tan1)))))
                             (sin1 (* -1.0 tan1 cos1)))
                        (m3-fudge-columnwise r 1 2 sin1 cos1)
                        (setf (v3-x s) (m3 a 0 0)
                              (v3-y s) (- (* cos0 cos1 (m3 a 1 1)) (* sin1 (prod0 cos0 sin0 (m3 a 1 2) (m3 a 2 2))))
                              (v3-z s) (+ (* sin0 sin1 (m3 a 1 1)) (* cos1 (prod1 cos0 sin0 (m3 a 1 2) (m3 a 2 2)))))
                        (break))))
                (if test2
                    ;; 2x2 closed form factorisation
                    (let* ((tmp (/ (+ (m3** a 0 0) (- (m3** a 0 1)) (m3** a 1 1))
                                   (m3* a 0 1 1 1)))
                           (tan0 (* 0.5 (+ (- tmp) (sqrt (+ 4.0 (* tmp tmp))))))
                           (cos0 (/ 1.0 (sqrt (+ 1.0 (* tan0 tan0)))))
                           (sin0 (* tan0 cos0)))
                      (m3-fudge-rowwise l 0 1 sin0 cos0)
                      (let* ((tan1 (/ (- (m3 a 0 1) (* tan0 (m3 a 1 1))) (m3 a 0 0)))
                             (cos1 (/ 1.0 (sqrt (+ 1.0 (* tan1 tan1)))))
                             (sin1 (* -1.0 tan1 cos1)))
                        (m3-fudge-columnwise r 0 1 sin1 cos1)
                        (setf (v3-x s) (- (* cos0 cos1 (m3 a 0 0)) (* sin1 (prod0 cos0 sin0 (m3 a 0 1) (m3 a 1 1))))
                              (v3-y s) (+ (* sin0 sin1 (m3 a 0 0)) (* cos1 (prod1 cos0 sin0 (m3 a 0 1) (m3 a 1 1))))
                              (v3-z s) (m3 a 2 2))
                        (break)))
                    (m3-golub-kahan-step a l r))))))
  (with-v3-accessor (v3v s)
    (dotimes (row 3)
      (when (< (v3v row) 0.0)
        (setf (v3v row) (- (v3v row)))
        (dotimes (col 3)
          (setf (m3 r row col) (- (m3 r row col))))))))

(defun m3-singular-value-composition (a l s r)
  (let ((mtemp (make-m3)))
    ;; product S*R
    (with-v3-accessor (v3v s)
      (dotimes (row 3)
        (dotimes (col 3)
          (setf (m3 mtemp row col) (* (v3v row) (m3 r row col))))))
    ;; product L*S*R
    (dotimes (row 3)
      (dotimes (col 3)
        (setf (m3 a row col) 0.0)
        (dotimes (mid 3)
          (incf (m3 a row col) (* (m3 l row mid) (m3 mtemp mid col))))))))

(defun m3-column-ilength (m col)
  (/ 1.0 (sqrt (+ (m3** m 0 col) (m3** m 1 col) (m3** m 2 col)))))

(defun m3-orthonormalise (m)
  "Algorithm uses Gram-Schmidt orthogonalisation.  If M is
M = [m0|m1|m2], then orthonormal output matrix is Q = [q0|q1|q2],

  q0 = m0/|m0|
  q1 = (m1-(q0*m1)q0)/|m1-(q0*m1)q0|
  q2 = (m2-(q0*m2)q0-(q1*m2)q1)/|m2-(q0*m2)q0-(q1*m2)q1|

where |V| indicates length of vector V and A*B indicates dot
product of vectors A and B."
  ;; compute q0
  (let ((ilength (m3-column-ilength m 0)))
    (setf (m3 m 0 0) (* ilength (m3 m 0 0))
          (m3 m 1 0) (* ilength (m3 m 1 0))
          (m3 m 2 0) (* ilength (m3 m 2 0))))
  ;; compute q1
  (let ((dot0 (+ (m3* m 0 0 0 1) (m3* m 1 0 1 1) (m3* m 2 0 2 1))))
    (decf (m3 m 0 1) (* dot0 (m3 m 0 0)))
    (decf (m3 m 1 1) (* dot0 (m3 m 1 0)))
    (decf (m3 m 2 1) (* dot0 (m3 m 2 0))))
  (let ((ilength (m3-column-ilength m 1)))
    (setf (m3 m 0 1) (* ilength (m3 m 0 1))
          (m3 m 1 1) (* ilength (m3 m 1 1))
          (m3 m 2 1) (* ilength (m3 m 2 1))))
  ;; compute q2
  (let ((dot1 (+ (m3* m 0 1 0 2) (m3* m 1 1 1 2) (m3* m 2 1 2 2)))
        (dot0 (+ (m3* m 0 0 0 2) (m3* m 1 0 1 2) (m3* m 2 0 2 2))))
    (decf (m3 m 0 2) (+ (* dot0 (m3 m 0 0)) (* dot1 (m3 m 0 1))))
    (decf (m3 m 1 2) (+ (* dot0 (m3 m 1 0)) (* dot1 (m3 m 1 1))))
    (decf (m3 m 2 2) (+ (* dot0 (m3 m 2 0)) (* dot1 (m3 m 2 1)))))
  (let ((ilength (m3-column-ilength m 2)))
    (setf (m3 m 0 2) (* ilength (m3 m 0 2))
          (m3 m 1 2) (* ilength (m3 m 1 2))
          (m3 m 2 2) (* ilength (m3 m 2 2)))))

(defun m3-qdu-decomposition (m q d u)
  "Factor M = QR = QDU where Q is orthogonal, D is diagonal,
and U is upper triangular with ones on its diagonal.  Algorithm uses
Gram-Schmidt orthogonalization (the QR algorithm).

If M = [ m0 | m1 | m2 ] and Q = [ q0 | q1 | q2 ], then

  q0 = m0/|m0|
  q1 = (m1-(q0*m1)q0)/|m1-(q0*m1)q0|
  q2 = (m2-(q0*m2)q0-(q1*m2)q1)/|m2-(q0*m2)q0-(q1*m2)q1|

where |V| indicates length of vector V and A*B indicates dot
product of vectors A and B.  The matrix R has entries

  r00 = q0*m0  r01 = q0*m1  r02 = q0*m2
  r10 = 0      r11 = q1*m1  r12 = q1*m2
  r20 = 0      r21 = 0      r22 = q2*m2

so D = diag(r00,r11,r22) and U has entries u01 = r01/r00,
u02 = r02/r00, and u12 = r12/r11.

Q = rotation
D = scaling
U = shear

D stores the three diagonal entries r00, r11, r22
U stores the entries U[0] = u01, U[1] = u02, U[2] = u12"
  ;; build orthogonal matrix Q
  (let ((ilength (m3-column-ilength m 0)))
    (setf (m3 q 0 0) (* ilength (m3 m 0 0))
          (m3 q 1 0) (* ilength (m3 m 1 0))
          (m3 q 2 0) (* ilength (m3 m 2 0))))
  (let ((dot (+ (* (m3 q 0 0) (m3 m 0 1)) (* (m3 q 1 0) (m3 m 1 1)) (* (m3 q 2 0) (m3 m 2 1)))))
    (setf (m3 q 0 1) (- (m3 m 0 1) (* dot (m3 q 0 0)))
          (m3 q 1 1) (- (m3 m 1 1) (* dot (m3 q 1 0)))
          (m3 q 2 1) (- (m3 m 2 1) (* dot (m3 q 2 0)))))
  (let ((ilength (m3-column-ilength q 1)))
    (setf (m3 q 0 1) (* ilength (m3 q 0 1))
          (m3 q 1 1) (* ilength (m3 q 1 1))
          (m3 q 2 1) (* ilength (m3 q 2 1))))
  (let ((dot (+ (* (m3 q 0 0) (m3 m 0 2)) (* (m3 q 1 0) (m3 m 1 2)) (* (m3 q 2 0) (m3 m 2 2)))))
    (setf (m3 q 0 2) (- (m3 m 0 2) (* dot (m3 q 0 0)))
          (m3 q 1 2) (- (m3 m 1 2) (* dot (m3 q 1 0)))
          (m3 q 2 2) (- (m3 m 2 2) (* dot (m3 q 2 0)))))
  (let ((dot (+ (* (m3 q 0 1) (m3 m 0 2)) (* (m3 q 1 1) (m3 m 1 2)) (* (m3 q 2 1) (m3 m 2 2)))))
    (decf (m3 q 0 2) (* dot (m3 q 0 1)))
    (decf (m3 q 1 2) (* dot (m3 q 1 1)))
    (decf (m3 q 2 2) (* dot (m3 q 2 1))))
  (let ((ilength (m3-column-ilength q 2)))
    (setf (m3 q 0 2) (* ilength (m3 q 0 2))
          (m3 q 1 2) (* ilength (m3 q 1 2))
          (m3 q 2 2) (* ilength (m3 q 2 2))))
  ;; guarantee that orthogonal matrix has determinant 1 (no reflections)
  (let ((det (+ (m3*3 q 0 0 1 1 2 2) (m3*3 q 0 1 1 2 2 0) (m3*3 q 0 2 1 0 2 1)
                (- (m3*3 q 0 2 1 1 2 0)) (- (m3*3 q 0 1 1 0 2 2)) (- (m3*3 q 0 0 1 2 2 1)))))
    (when (< det 0.0)
      (dotimes (row 3)
        (dotimes (col 3)
          (setf (m3 q row col) (- (m3 q row col)))))))
  ;; build "right" matrix R
  (let ((r00 (+ (* (m3 q 0 0) (m3 m 0 0)) (* (m3 q 1 0) (m3 m 1 0)) (* (m3 q 2 0) (m3 m 2 0))))
        (r01 (+ (* (m3 q 0 0) (m3 m 0 1)) (* (m3 q 1 0) (m3 m 1 1)) (* (m3 q 2 0) (m3 m 2 1))))
        (r11 (+ (* (m3 q 0 1) (m3 m 0 1)) (* (m3 q 1 1) (m3 m 1 1)) (* (m3 q 2 1) (m3 m 2 1))))
        (r02 (+ (* (m3 q 0 0) (m3 m 0 2)) (* (m3 q 1 0) (m3 m 1 2)) (* (m3 q 2 0) (m3 m 2 2))))
        (r12 (+ (* (m3 q 0 1) (m3 m 0 2)) (* (m3 q 1 1) (m3 m 1 2)) (* (m3 q 2 1) (m3 m 2 2))))
        (r22 (+ (* (m3 q 0 2) (m3 m 0 2)) (* (m3 q 1 2) (m3 m 1 2)) (* (m3 q 2 2) (m3 m 2 2)))))
    ;; the scaling component
    (setf (v3-x d) r00
          (v3-y d) r11
          (v3-z d) r22)
    ;; the shear component
    (let ((id0 (/ 1.0 (v3-x d))))
      (setf (v3-x u) (* r01 id0)
            (v3-y u) (* r02 id0)
            (v3-z u) (/ r12 (v3-y d))))))

(defun max-cubic-root (c0 c1 c2)
  "Spectral norm is for A^T*A, so characteristic polynomial
P(x) = c[0]+c[1]*x+c[2]*x^2+x^3 has three positive real roots.
This yields the assertions c[0] < 0 and c[2]*c[2] >= 3*c[1]."
  (flet ((poly (x) (+ c0 (* x (+ c1 (* x (+ c2 x)))))))
    ;; quick out for uniform scale (triple root)
    (let ((one-third (/ 1.0 3.0))
          (discr (- (* c2 c2) (* 3.0 c1))))
      (when (< discr epsilon)
        (return-from max-cubic-root (- (* one-third c2)))))
    ;; Compute an upper bound on roots of P(x).  This assumes that A^T*A
    ;; has been scaled by its largest entry.
    (let ((x 1.0))
      (when (minusp (poly x))
        ;; uses a matrix norm to find an upper bound on maximum root
        (setf x (abs c0))
        (let ((tmp (+ 1.0 (abs c1))))
          (when (> tmp x)
            (setf x tmp)))
        (let ((tmp (+ 1.0 (abs c2))))
          (when (> tmp x)
            (setf x tmp))))
      ;; Newton's method to find root
      (let ((2c2 (* 2.0 c2)))
        (iter (repeat 16)
              (for poly = (poly x))
              (when (< (abs poly) epsilon)
                (return))
              (let ((deriv (+ c1 (* x (+ 2c2 (* 3.0 x))))))
                (decf x (/ poly deriv))))
        x))))

(defun m3-spectral-norm (m)
  (let ((p (make-m3 (make-array 9 :element-type 'real :initial-element 0.0)))
        (pmax 0.0))
    (dotimes (row 3)
      (dotimes (col 3)
        (dotimes (mid 3)
          (incf (m3 p row col) (m3* m mid row mid col)))
        (maxf pmax (m3 p row col))))
    (let ((ipmax (/ 1.0 pmax)))
      (dotimes (row 3)
        (dotimes (col 3)
          (setf (m3 p row col) (* ipmax (m3 p row col))))))
    (let ((c0 (- (+ (* (m3 p 0 0) (- (m3* p 1 1 2 2) (m3* p 1 2 2 1)))
                    (* (m3 p 0 1) (- (m3* p 2 0 1 2) (m3* p 1 0 2 2)))
                    (* (m3 p 0 2) (- (m3* p 1 0 2 1) (m3* p 2 0 1 1))))))
          (c1 (+ (- (m3* p 0 0 1 1) (m3* p 0 1 1 0))
                 (- (m3* p 0 0 2 2) (m3* p 0 2 2 0))
                 (- (m3* p 1 1 2 2) (m3* p 1 2 2 1))))
          (c2 (- (+ (m3 p 0 0) (m3 p 1 1) (m3 p 2 2)))))
      (sqrt (* pmax (max-cubic-root c0 c1 c2))))))

(defun angle-axis<-m3 (m)
  "Let (x,y,z) be the unit-length axis and let A be an angle of rotation.
The rotation matrix is R = I + sin(A)*P + (1-cos(A))*P^2 where
I is the identity and

      +-        -+
  P = |  0 -z +y |
      | +z  0 -x |
      | -y +x  0 |
      +-        -+

If A > 0, R represents a counterclockwise rotation about the axis in
the sense of looking from the tip of the axis vector towards the
origin.  Some algebra will show that

  cos(A) = (trace(R)-1)/2  and  R - R^t = 2*sin(A)*P

In the event that A = pi, R-R^t = 0 which prevents us from extracting
the axis through P.  Instead note that R = I+2*P^2 when A = pi, so
P^2 = (R-I)/2.  The diagonal entries of P^2 are x^2-1, y^2-1, and
z^2-1.  We can solve these for axis (x,y,z).  Because the angle is pi,
it does not matter which sign you choose on the square roots."
  (let* ((trace (+ (m3 m 0 0) (m3 m 1 1) (m3 m 2 2)))
         (cos (* 0.5 (- trace 1.0)))
         (angle (acos cos)))
    (values angle
            (if (> angle 0.0)
                (if (< angle pi)
                    (v3-normalisef (make-v3 (- (m3 m 2 1) (m3 m 1 2))
                                            (- (m3 m 0 2) (m3 m 2 0))
                                            (- (m3 m 1 0) (m3 m 0 1))))
                    ;; angle is pi
                    (if (>= (m3 m 0 0) (m3 m 1 1))
                        (if (>= (m3 m 0 0) (m3 m 2 2))
                            ;; r00 is maximum diagonal term
                            (let* ((x (* 0.5 (sqrt (- (m3 m 0 0) (m3 m 1 1) (m3 m 2 2) -1.0))))
                                   (ihalf (/ 0.5 x)))
                              (make-v3 x (* ihalf (m3 m 0 1)) (* ihalf (m3 m 0 2))))
                            ;; r22 is maximum diagonal term
                            (let* ((z (* 0.5 (sqrt (- (m3 m 2 2) (m3 m 0 0) (m3 m 1 1) -1.0))))
                                   (ihalf (/ 0.5 z)))
                              (make-v3 (* ihalf (m3 m 0 2)) (* ihalf (m3 m 1 2)) z)))
                        (if (>= (m3 m 1 1) (m3 m 2 2))
                            ;; r11 is maximum diagonal term
                            (let* ((y (* 0.5 (sqrt (- (m3 m 1 1) (m3 m 0 0) (m3 m 2 2) -1.0))))
                                   (ihalf (/ 0.5 y)))
                              (make-v3 (* ihalf (m3 m 0 1)) y (* ihalf (m3 m 1 2))))
                            ;; r22 is maximum diagonal term
                            (let* ((z (* 0.5 (sqrt (- (m3 m 2 2) (m3 m 0 0) (m3 m 1 1) -1.0))))
                                   (ihalf (/ 0.5 z)))
                              (make-v3 (* ihalf (m3 m 0 2)) (* ihalf (m3 m 1 2)) z)))))
                ;; The angle is 0 and the matrix is the identity.  Any axis will
                ;; work, so just use the x-axis.
                (make-v3 1.0 0.0 0.0)))))

(defun m3<-angle-axis (angle v)
  (let* ((cos (cos angle))
         (sin (sin angle))
         (1-cos (- 1.0 cos))
         (x^2 (* (v3-x v) (v3-x v)))
         (y^2 (* (v3-y v) (v3-y v)))
         (z^2 (* (v3-z v) (v3-z v)))
         (xym (* (v3-x v) (v3-y v) 1-cos))
         (xzm (* (v3-x v) (v3-z v) 1-cos))
         (yzm (* (v3-y v) (v3-z v) 1-cos))
         (xsin (* sin (v3-x v)))
         (ysin (* sin (v3-y v)))
         (zsin (* sin (v3-z v))))
    (make-m3* (+ cos (* x^2 1-cos)) (+ xym zsin) (- xzm ysin)
              (- xym zsin) (+ cos (* y^2 1-cos)) (+ yzm xsin)
              (+ xzm ysin) (- yzm xsin) (+ cos (* z^2 1-cos)))))

(defmacro frob-angles<-m3 (angles psign pr pc y0sign y0r1 y0c1 y0r2 y0c2 rsign rr1 rc1 rr2 rc2 y1sign y1r1 y1c1 y1r2 y1c2)
  `(defun ,(format-symbol t "ANGLES-~A<-M3" angles) (m)
     (let ((p (asin (,psign (m3 m ,pr ,pc)))))
       (if (< p (* pi 0.5))
           (if (> p (* pi -0.5))
               (values t
                       (atan (,y0sign (m3 m ,y0r1 ,y0c1)) (m3 m ,y0r2 ,y0c2))
                       p
                       (atan (,rsign (m3 m ,rr1 ,rc1)) (m3 m ,rr2 ,rc2)))
               ;; WARNING.  Not a unique solution.
               (let ((r 0.0))           ; any angle works
                 (values nil
                         (- r (atan (atan (,y1sign (m3 m ,y1r1 ,y1c1)) (m3 m ,y1r2 ,y1c2))))
                         p
                         r)))
           ;; WARNING.  Not a unique solution.
           (let ((r 0.0))               ; any angle works
             (values nil
                     (- (atan (atan (,y1sign (m3 m ,y1r1 ,y1c1)) (m3 m ,y1r2 ,y1c2))) r)
                     p
                     r))))))

;; rot =  cy*cz          -cy*sz           sy
;;        cz*sx*sy+cx*sz  cx*cz-sx*sy*sz -cy*sx
;;       -cx*cz*sy+sx*sz  cz*sx+cx*sy*sz  cx*cy
(frob-angles<-m3 xyz + 0 2 - 1 2 2 2 - 0 1 0 0 + 1 0 0 0)
;; rot =  cy*cz          -sz              cz*sy
;;        sx*sy+cx*cy*sz  cx*cz          -cy*sx+cx*sy*sz
;;       -cx*sy+cy*sx*sz  cz*sx           cx*cy+sx*sy*sz
(frob-angles<-m3 xzy - 0 1 + 2 1 1 1 + 0 2 0 0 - 2 0 2 2)
;; rot =  cy*cz+sx*sy*sz  cz*sx*sy-cy*sz  cx*sy
;;        cx*sz           cx*cz          -sx
;;       -cz*sy+cy*sx*sz  cy*cz*sx+sy*sz  cx*cy
(frob-angles<-m3 yxz - 1 2 + 0 2 2 2 + 1 0 1 1 - 0 1 0 0)
;; rot =  cy*cz           sx*sy-cx*cy*sz  cx*sy+cy*sx*sz
;;        sz              cx*cz          -cz*sx
;;       -cz*sy           cy*sx+cx*sy*sz  cx*cy-sx*sy*sz
(frob-angles<-m3 yzx + 1 0 - 2 0 0 0 - 1 2 1 1 + 2 1 2 2)
;; rot =  cy*cz-sx*sy*sz -cx*sz           cz*sy+cy*sx*sz
;;        cz*sx*sy+cy*sz  cx*cz          -cy*cz*sx+sy*sz
;;       -cx*sy           sx              cx*cy
(frob-angles<-m3 zxy + 2 1 - 0 1 1 1 - 2 0 2 2 + 0 2 0 0)
;; rot =  cy*cz           cz*sx*sy-cx*sz  cx*cz*sy+sx*sz
;;        cy*sz           cx*cz+sx*sy*sz -cz*sx+cx*sy*sz
;;       -sy              cy*sx           cx*cy
(frob-angles<-m3 zyx - 2 0 + 1 0 0 0 + 2 1 2 2 - 0 1 0 2)

(defun m3<-angle-x (x)
  (let ((cos (cos x)) (sin (sin x)))
    (make-m3 1.0 0.0 0.0 0.0 cos sin 0.0 (- sin) cos)))

(defun m3<-angle-y (y)
  (let ((cos (cos y)) (sin (sin y)))
    (make-m3 cos 0.0 (- sin) 0.0 1.0 0.0 sin 0 cos)))

(defun m3<-angle-z (z)
  (let ((cos (cos z)) (sin (sin z)))
    (make-m3 cos sin 0.0 (- sin) cos 0.0 0.0 0.0 1.0)))

(defun m3<-angles-xyz (y p r) (mult (m3<-angle-x y) (mult (m3<-angle-y p) (m3<-angle-z r))))
(defun m3<-angles-xzy (y p r) (mult (m3<-angle-x y) (mult (m3<-angle-z p) (m3<-angle-y r))))
(defun m3<-angles-yxz (y p r) (mult (m3<-angle-y y) (mult (m3<-angle-x p) (m3<-angle-z r))))
(defun m3<-angles-yzx (y p r) (mult (m3<-angle-y y) (mult (m3<-angle-z p) (m3<-angle-x r))))
(defun m3<-angles-zxy (y p r) (mult (m3<-angle-z y) (mult (m3<-angle-x p) (m3<-angle-y r))))
(defun m3<-angles-zyx (y p r) (mult (m3<-angle-z y) (mult (m3<-angle-y p) (m3<-angle-x r))))

(defun m3-tridiagonal (m diag subdiag)
  "Householder reduction T = Q^t M Q
  Input:
    m, symmetric 3x3 matrix M
  Output:
    m, orthogonal matrix Q
    diag, diagonal entries of T
    subd, subdiagonal entries of T (T is symmetric)"
  (let ((a (m3 m 0 0)) (b (m3 m 0 1)) (c (m3 m 0 2))
        (d (m3 m 1 1)) (e (m3 m 1 2)) (f (m3 m 2 2)))
    (if (>= (abs c) epsilon)
        (let* ((length (sqrt (+ (* b b) (* c c))))
               (ilength (/ 1.0 length)))
          (setf b (* b ilength)
                c (* c ilength))
          (let ((q (+ (* 2.0 b e) (* c (- f d)))))
            (setv m #(1.0 0.0 0.0 0.0 b c 0 c (- b)))
            (setf (v3-x diag) a
                  (v3-y diag) (+ d (* c q))
                  (v3-z diag) (- f (* c q))
                  (v3-x subdiag) length
                  (v3-y subdiag) (- e (* b q))
                  (v3-z subdiag) 0.0)
            (values diag subdiag)))
        (progn
          (setv m *m3-identity*)
          (setf (v3-x diag) a
                (v3-y diag) d
                (v3-z diag) f
                (v3-x subdiag) b
                (v3-y subdiag) e
                (v3-z subdiag) 0.0)
          (values diag subdiag)))))

;;;
;;; the third loop is weird in the original
;;; action: simplified with semantics retained
;;;
(defun m3-ql-algorithm (m diag subdiag)
  "QL iteration with implicit shifting to reduce matrix from tridiagonal
to diagonal."
  (let ((successp t))
    (with-v3-accessor (diag diag)
      (with-v3-accessor (subdiag subdiag)
        (iter (for i0 from 0 below 3)
              (let ((maxiter 32))
                (iter level-two
                      (with i1)
                      (for iter below maxiter)
                      (iter (for si1 from i0 to 1)
                            (when (and (zerop (subdiag si1))
                                       (= si1 i0))
                              (when (= iter (1- maxiter))
                                (setf successp nil))
                              (finally (setf i1 si1))
                              (return-from level-two)))
                      (let* ((tmp0 (/ (- (diag (1+ i0)) (diag i0)) 
                                      (* 2.0 (subdiag i0))))
                             (tmp1 (sqrt (+ 1.0 (* tmp0 tmp0))))
                             (tmp0 (if (minusp tmp0)
                                       (+ (diag i1) (- (diag i0)) (/ (subdiag i0) (- tmp0 tmp1)))
                                       (+ (diag i1) (- (diag i0)) (/ (subdiag i0) (+ tmp0 tmp1))))))
                        (let ((sin 1.0) (cos 1.0) (tmp2 0.0))
                          (iter (for i2 from (1- i1) above (1- i0))
                                (let ((tmp3 (* sin (subdiag i2)))
                                      (tmp4 (* cos (subdiag i2))))
                                  (if (>= (abs tmp3) (abs tmp0))
                                      (setf cos (/ tmp0 tmp3)
                                            tmp1 (sqrt (+ 1.0 (* cos cos)))
                                            (subdiag (1+ i2)) (* tmp3 tmp1)
                                            sin (/ 1.0 tmp1)
                                            cos (* cos sin))
                                      (setf sin (/ tmp3 tmp0)
                                            tmp1 (sqrt (+ 1.0 (* sin sin)))
                                            (subdiag (1+ i2)) (* tmp0 tmp1)
                                            cos (/ 1.0 tmp1)
                                            sin (* sin cos)))
                                  (setf tmp0 (- (diag (1+ i2)) tmp2)
                                        tmp1 (+ (* (- (diag i2) tmp0) sin) (* 2.0 tmp4 cos))
                                        tmp2 (* sin tmp1)
                                        (diag (1+ i2)) (+ tmp0 tmp2)
                                        tmp0 (- (* cos tmp1) tmp4))
                                  (dotimes (row 3)
                                    (setf tmp3 (m3 m row (1+ i2))
                                          (m3 m row (1+ i2)) (+ (* sin (m3 m row i2)) (* cos tmp3))
                                          (m3 m row i2) (+ (* cos (m3 m row i2)) (* sin tmp3))))))
                          (decf (diag i0) tmp2)
                          (setf (subdiag i0) tmp0
                                (subdiag i1) 0.0)))
                      (when (= iter (1- maxiter))
                        (setf successp nil)))))))
    successp))

(defun m3-eigensolve-symmetric (m eigenvalue eigenvectors)
  (declare (matrix3 m) (vector3 eigenvalue) ((array vector3 (3)) eigenvectors))
  (let ((k (copy-m3 m))
        (subdiag (make-v3 0.0 0.0 0.0)))
    (m3-tridiagonal k eigenvalue subdiag)
    (m3-ql-algorithm m eigenvalue subdiag)
    (dotimes (i 3)
      (m3-extract-column k i (aref eigenvectors i)))
    ;; make eigenvectors form a right-handed system
    (let ((det (dot (aref eigenvectors 0) (v3-cross (aref eigenvectors 1) (aref eigenvectors 2)))))
      (when (minusp det)
        (v3-negf (aref eigenvectors 2))))))

(defun m3-tensor-productf (m u v)
  (with-v3-accessor (u u)
    (with-v3-accessor (v v)
      (dotimes (row 3)
        (dotimes (col 3)
          (setf (m3 m row col) (* (u row) (v col))))))))

;;; matrix4
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

(defun m4* (m r1 c1 r2 c2)
  (let ((a (m4-a m)))
    (* (aref a (+ r1 (ash c1 2)))
       (aref a (+ r2 (ash c2 2))))))

(defun m4** (m r c)
  (let ((ref (aref (m4-a m) (+ r (ash c 2)))))
    (* ref ref)))

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
  (- (* (m4 m r0 c0) (- (m4* m r1 c1 r2 c2) (m4* m r2 c1 r1 c2)))
     (* (m4 m r0 c1) (- (m4* m r1 c0 r2 c2) (m4* m r2 c0 r1 c2)))
     (- (* (m4 m r0 c2) (- (m4* m r1 c0 r2 c1) (m4* m r2 c0 r1 c1))))))

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
  (let ((iw (/ 1.0 (+ (* (m4 m 3 0) (v3-x v)) (* (m4 m 3 1) (v3-y v)) (* (m4 m 3 2) (v3-z v)) (m4 m 3 3)))))
    (make-v3 (* iw (+ (* (m4 m 0 0) (v3-x v)) (* (m4 m 0 1) (v3-y v)) (* (m4 m 0 2) (v3-z v)) (m4 m 0 3)))
             (* iw (+ (* (m4 m 1 0) (v3-x v)) (* (m4 m 1 1) (v3-y v)) (* (m4 m 1 2) (v3-z v)) (m4 m 1 3)))
             (* iw (+ (* (m4 m 2 0) (v3-x v)) (* (m4 m 2 1) (v3-y v)) (* (m4 m 2 2) (v3-z v)) (m4 m 2 3))))))

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
        (let ((idet (/ 1 (+ (* t00 m00) (* t10 m01) (* t20 m02) (* t30 m03)))))
          (let ((d00 (* t00 idet))
                (d10 (* t10 idet))
                (d20 (* t20 idet))
                (d30 (* t30 idet))
                (d01 (* (- (+ (- (* v5 m01) (* v4 m02)) (* v3 m03))) idet))
                (d11 (* (+ (+ (- (* v5 m00) (* v2 m02)) (* v1 m03))) idet))
                (d21 (* (- (+ (- (* v4 m00) (* v2 m01)) (* v0 m03))) idet))
                (d31 (* (+ (+ (- (* v3 m00) (* v1 m01)) (* v0 m02))) idet)))
            (let ((v0 (- (* m10 m31) (* m11 m30)))
                  (v1 (- (* m10 m32) (* m12 m30)))
                  (v2 (- (* m10 m33) (* m13 m30)))
                  (v3 (- (* m11 m32) (* m12 m31)))
                  (v4 (- (* m11 m33) (* m13 m31)))
                  (v5 (- (* m12 m33) (* m23 m32))))
              (let ((d02 (* (+ (+ (- (* v5 m01) (* v4 m02)) (* v3 m03))) idet))
                    (d12 (* (- (+ (- (* v5 m00) (* v2 m02)) (* v1 m03))) idet))
                    (d22 (* (+ (+ (- (* v4 m00) (* v2 m01)) (* v0 m03))) idet))
                    (d32 (* (- (+ (- (* v3 m00) (* v1 m01)) (* v0 m02))) idet)))
                (let ((v0 (- (* m10 m21) (* m11 m20)))
                      (v1 (- (* m10 m22) (* m12 m20)))
                      (v2 (- (* m10 m23) (* m13 m20)))
                      (v3 (- (* m11 m22) (* m12 m21)))
                      (v4 (- (* m11 m23) (* m13 m21)))
                      (v5 (- (* m12 m23) (* m23 m22))))
                  (let ((d03 (* (- (+ (- (* v5 m01) (* v4 m02)) (* v3 m03))) idet))
                        (d13 (* (+ (+ (- (* v5 m00) (* v2 m02)) (* v1 m03))) idet))
                        (d23 (* (- (+ (- (* v4 m00) (* v2 m01)) (* v0 m03))) idet))
                        (d33 (* (+ (+ (- (* v3 m00) (* v1 m01)) (* v0 m02))) idet)))
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
      (let ((idet (/ 1 (+ (* m00 t00) (* m01 t10) (* m02 t20)))))
        (let ((t00 (* t00 idet)) (t10 (* t10 idet)) (t20 (* t20 idet))
              (m00 (* m00 idet)) (m01 (* m01 idet)) (m02 (* m02 idet)))
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

(defmethod mult ((m matrix4) (p normal-plane))
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
  (not (and (real= (+ (m4** m 0 0) (m4** m 1 0) (m4** m 2 0)) 1.0 0.0001)
            (real= (+ (m4** m 0 1) (m4** m 1 1) (m4** m 2 1)) 1.0 0.0001)
            (real= (+ (m4** m 0 2) (m4** m 1 2) (m4** m 2 2)) 1.0 0.0001))))

(defun m4-negative-scalep (m)
  (minusp (m4-determinant m)))

(defun m3<-axes (a0 a1 a2)
  (make-m3* (v3-x a0) (v3-y a0) (v3-z a0)
            (v3-x a1) (v3-y a1) (v3-z a1)
            (v3-x a2) (v3-y a2) (v3-z a2)))

(defun axes<-m3 (m)
  (let ((a (m3-a m)))
    (values (make-v3 (aref a 0) (aref a 1) (aref a 2))
            (make-v3 (aref a 3) (aref a 4) (aref a 5))
            (make-v3 (aref a 6) (aref a 7) (aref a 8)))))

(defun q<-m3 (m)
  ;; Algorithm in Ken Shoemake's article in 1987 SIGGRAPH course notes
  ;; article "Quaternion Calculus and Fast Animation".
  (let ((trace (+ (m3 m 0 0) (m3 m 1 1) (m3 m 2 2))))
    (if (plusp trace)
        ;; |w| > 1/2, may as well choose w > 1/2
        (let* ((root (sqrt (1+ trace)))
               (w (* 0.5 root))
               (root (/ 0.5 root)))
          (make-q w
                  (* root (- (m3 m 2 1) (m3 m 1 2)))
                  (* root (- (m3 m 0 2) (m3 m 2 0)))
                  (* root (- (m3 m 1 0) (m3 m 0 1)))))
        (let ((i 0) (inext #(1 2 0)))
          (when (> (m3 m 1 1) (m3 m 0 0)) (setf i 1))
          (when (> (m3 m 2 2) (m3 m i i)) (setf i 2))
          (let* ((j (aref inext i))
                 (k (aref inext j)))
            (let ((root (sqrt (- (m3 m i i) (m3 m j j) (m3 m k k) -1.0)))
                  x y z)
              (flet ((qset (i val) (case i
                                     (0 (setf x val))
                                     (1 (setf y val))
                                     (2 (setf z val)))))
                (qset i (* 0.5 root))
                (let* ((root (/ 0.5 root))
                       (w (* root (- (m3 m k j) (m3 m j k)))))
                  (qset j (* root (+ (m3 m j i) (m3 m i j))))
                  (qset k (* root (+ (m3 m k i) (m3 m i k))))
                  (make-q w x y z)))))))))

(defun m3<-q (q)
  (let ((x (* 2 (q-x q))) (y (* 2 (q-y q))) (z (* 2 (q-z q))))
    (let ((wx (* x (q-w q))) (wy (* y (q-w q))) (wz (* z (q-w q)))
          (xx (* x (q-x q))) (xy (* y (q-x q))) (xz (* z (q-x q)))
          (yy (* y (q-y q))) (yz (* z (q-y q)))
          (zz (* z (q-z q))))
      (make-m3* (- 1 yy zz) (+ xy wz) (- xz wy)
                (- xy wz) (- 1 xx zz) (+ yz wx)
                (- yz wx) (+ xz wy) (- 1 xx yy)))))

(defun q<-angle-axis (angle v)
  "V is unit length.
The quaternion representing the rotation is
q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)"
  (let* ((half-angle (* angle 0.5))
         (sin (sin half-angle)))
    (make-q (cos half-angle)
            (* sin (v3-x v))
            (* sin (v3-y v))
            (* sin (v3-z v)))))

(defun angle-axis<-q (q)
  "The quaternion representing the rotation is
q = cos(A/2)+sin(A/2)*(x*i+y*j+z*k)"
  (let ((length-sq (+ (* (q-x q) (q-x q)) (* (q-y q) (q-y q)) (* (q-z q) (q-z q)))))
    (if (> length-sq 0.0)
        (let ((ilength (/ 1 (sqrt length-sq))))
          (values (* 2 (acos (q-w q)))
                  (make-v3 (* ilength (q-x q))
                           (* ilength (q-y q))
                           (* ilength (q-z q)))))
        ;; angle is 0 (mod 2*pi), so any axis will do
        (values 0.0
                (copy-vector3 *v3-unit-x*)))))

(defun q<-axes (a0 a1 a2)
  (q<-m3 (m3<-axes a0 a1 a2)))

(defun axes<-q (q)
  (axes<-m3 (m3<-q q)))

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
  (let ((itranslation (v3-neg translation))
        (iscale (make-v3 (/ 1 (v3-x scale)) (/ 1 (v3-y scale)) (/ 1 (v3-z scale))))
        (iorientation (q-invert orientation)))
    ;; Because we're inverting, order is translation, rotation, scale
    ;; So make translation relative to scale & rotation
    (let ((itranslation (mult iorientation (mult itranslation iscale))))
      (lret ((m (m4<-m3 (mult (m3-make-scale iscale) (m3<-q iorientation)))))
        (setf (m4-trans m) itranslation
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

(defmethod intersects-p ((aab1 axis-aligned-box) (aab2 axis-aligned-box) &key &allow-other-keys)
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

(defmethod intersects-p ((r ray) (aab axis-aligned-box) &key full &allow-other-keys)
  "This method looks crappy to my untrained eye."
  (cond ((aab-null-p aab) nil)
        ((aab-infinite-p aab) (values t 0.0 (+infinity)))
        ((and (not full) (v3<= (ray-orig r) (aab-max aab)) (v3>= (ray-orig r) (aab-min aab)))
         (values t 0.0))
        (t (let ((min (aab-min aab)) (max (aab-max aab))
                 (orig (ray-orig r)) (dir (ray-dir r)))
             (with-v3-accessor (vmin min)
               (with-v3-accessor (vmax max)
                 (with-v3-accessor (orig orig)
                   (with-v3-accessor (dir dir)
                     (if full
                         ;; Full, two-point intersection
                         (with-v3-accessor (absdir (v3-abs dir))
                           ;; Sort the axis, ensure check minimise floating error axis first
                           (let ((imax 0) (imid 1) (imin 2))
                             (when (< (absdir 0) (absdir 2))
                               (setf imax 2 imin 0))
                             (if (< (absdir 1) (absdir imin))
                                 (setf imid imin imin 1)
                                 (when (> (absdir 1) (absdir imax))
                                   (setf imid imax imax 1)))
                             (let ((start 0) (end (+infinity)))
                               (flet ((calc-axis (i)
                                        (let* ((denom (/ 1.0 (dir i)))
                                               (newstart (* denom (- (vmin i) (orig i))))
                                               (newend (* denom (- (vmax i) (orig i)))))
                                          (when (> newstart newend)
                                            (rotatef newstart newend))
                                          (when (or (> newstart end) (< newend start))
                                            (return-from intersects-p nil))
                                          (maxf start newstart)
                                          (minf end newend))))
                                 ;; Check each axis in turn
                                 (calc-axis imax)
                                 (if (< (absdir imid) epsilon)
                                     ;; Parallel with middle and minimise axis, check bounds only
                                     (when (or (not (iboundp (orig imid) (vmin imid) (vmax imid)))
                                               (not (iboundp (orig imin) (vmin imin) (vmax imin))))
                                       (return-from intersects-p nil))
                                     (progn
                                       (calc-axis imid)
                                       (if (< (absdir imin) epsilon)
                                           ;; Parallel with minimise axis, check bounds only
                                           (when (not (iboundp (orig imin) (vmin imin) (vmax imin)))
                                             (return-from intersects-p nil))
                                           (calc-axis imin))))
                                 (values t start end)))))
                         ;; Simple, one-point intersection: check each face in turn, only check closest 3
                         (let ((hit nil) (lowt 0.0))
                           (flet ((check-plane (minp axis ax1 ax2)
                                    (let ((minmax (if minp (vmin axis) (vmax axis)))
                                          (orig (orig axis)) (dir (dir axis)))
                                      (cond ((and (<= orig minmax) (if minp (plusp dir) (minusp dir)))
                                             (let ((tee (/ (- minmax orig) dir)))
                                               (when (>= tee 0)
                                                 ;; Substitute t back into ray and check bounds and dist
                                                 (let ((hitpoint (v3+ orig (v3* dir tee))))
                                                   (with-v3-accessor (hit hitpoint)
                                                     (when (and (iboundp (hit ax1) (vmin ax1) (vmax ax1))
                                                                (iboundp (hit ax2) (vmin ax2) (vmax ax2))
                                                                (or (not hit) (< tee lowt)))
                                                       (setf hit t lowt tee)))))))))))
                             (check-plane t   0 1 2)
                             (check-plane nil 0 1 2)
                             (check-plane t   1 0 2)
                             (check-plane nil 1 0 2)
                             (check-plane t   2 1 0)
                             (check-plane nil 2 1 0)
                             (values hit lowt))))))))))))

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
    (:infinite (+infinity))))

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

(defmethod intersects-p ((aab axis-aligned-box) (s sphere) &key &allow-other-keys) (ni))

(defmethod intersects-p ((aab axis-aligned-box) (p plane) &key &allow-other-keys) (ni))

(defmethod intersects-p ((aab axis-aligned-box) (v vector3) &key &allow-other-keys)
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
    (:infinite (make-v3 (+infinity) (+infinity) (+infinity)))
    (:finite (v3- (aab-max aab) (aab-min aab)))))

(defun aab-halfsize (aab)
  (ecase (aab-extent aab)
    (:null *v3-zero*)
    (:infinite (make-v3 (+infinity) (+infinity) (+infinity)))
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
