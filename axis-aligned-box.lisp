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

(defclass sphere () ())
(defclass plane () ())

(defun ni () (error "~@<This function is not implemented.~:@>"))

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
  (let* ((q1 (q-from-angle-axis (* 2 pi (random 1.0)) v))
         (new-up (q* q1 up))
         ;; Finally rotate v by given angle around randomised up
         (q2 (q-from-angle-axis angle new-up)))
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
            (q-from-angle-axis pi fallback-axis)
            (let ((axis (v3-cross *v3-unit-x* v1)))
              (let ((axis (if (v3-zerop axis) (v3-cross *v3-unit-y* v1))))
                (q-from-angle-axis pi (v3-normalise axis)))))
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
      (aab-mergef aab (m4* m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmax))
      (aab-mergef aab (m4* m4 current-corner))

      (setf (v3-y current-corner) (v3-y oldmax))
      (aab-mergef aab (m4* m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmin))
      (aab-mergef aab (m4* m4 current-corner))

      (setf (v3-x current-corner) (v3-x oldmax))
      (aab-mergef aab (m4* m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmax))
      (aab-mergef aab (m4* m4 current-corner))

      (setf (v3-y current-corner) (v3-y oldmin))
      (aab-mergef aab (m4* m4 current-corner))

      (setf (v3-z current-corner) (v3-z oldmin))
      (aab-mergef aab (m4* m4 current-corner)))))

(defun aab-transform-affine (aab m4)
  "Transforms the box according to the affine matrix supplied.

By calling this method you get the axis-aligned box which
surrounds the transformed version of this box. Therefore each
corner of the box is transformed by the matrix, then the
extents are mapped back onto the axes to produce another
AABB. Useful when you have a local AABB for an object which
is then transformed.

The matrix must be an affine matrix. m4-affine-p."
  (assert (m4-affine-p m4))
  (when (aab-finite-p aab)
    (let* ((centre (aab-centre aab))
           (halfsize (aab-halfsize aab))
           (new-centre (transform-affine m4 centre))
           (new-halfsize (make-v3 (+ (* (abs (m4ref m4 0 0)) (v3-x halfsize))
                                     (* (abs (m4ref m4 0 1)) (v3-y halfsize))
                                     (* (abs (m4ref m4 0 2)) (v3-z halfsize)))
                                  (+ (* (abs (m4ref m4 1 0)) (v3-x halfsize))
                                     (* (abs (m4ref m4 1 1)) (v3-y halfsize))
                                     (* (abs (m4ref m4 1 2)) (v3-z halfsize)))
                                  (+ (* (abs (m4ref m4 2 0)) (v3-x halfsize))
                                     (* (abs (m4ref m4 2 1)) (v3-y halfsize))
                                     (* (abs (m4ref m4 2 2)) (v3-z halfsize))))))
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

(defvar *aab-null* (make-aab))
(defvar *aab-infinite* (make-aab :extent :infinite))
