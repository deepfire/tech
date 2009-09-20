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

(defclass auto-param-data-source ()
  ((lights :accessor apds-lights :type vector)
   (world-matrices :accessor apds-world-matrices :type vector)
   (world-matrix-count :accessor apds-world-matrix-count :type (integer 0))
   (world-view-matrix :accessor apds-world-view-matrix :type matrix4)
   (view-proj-matrix :accessor apds-view-proj-matrix :type matrix4)
   (world-view-proj-matrix :accessor apds-world-view-proj-matrix :type matrix4)
   (inverse-world-matrix :accessor apds-inverse-world-matrix :type matrix4)
   (inverse-world-view-matrix :accessor apds-inverse-world-view-matrix :type matrix4)
   (inverse-view-matrix :accessor apds-inverse-view-matrix :type matrix4)
   (inverse-transpose-world-matrix :accessor apds-inverse-transpose-world-matrix :type matrix4)
   (inverse-transpose-world-view-matrix :accessor apds-inverse-transpose-world-view-matrix :type matrix4)
   (camera-position :accessor apds-camera-position :type matrix4)
   (camera-position-object-space :accessor apds-camera-position-object-space :type matrix4)
   (texture-view-proj-matrix :accessor apds-texture-view-proj-matrix :type vector)
   (texture-world-view-proj-matrix :accessor apds-texture-world-view-proj-matrix :type vector)
   (spotlight-view-proj-matrix :accessor apds-spotlight-view-proj-matrix :type vector)
   (spotlight-world-view-proj-matrix :accessor apds-spotlight-world-view-proj-matrix :type vector)
   (shadow-cam-depth-ranges :accessor apds-shadow-cam-depth-ranges :type vector)
   (view-matrix :accessor apds-view-matrix :type matrix4)
   (projection-matrix :accessor apds-projection-matrix :type matrix4)
   (dir-light-extrusion-distance :accessor apds-dir-light-extrusion-distance :type matrix4)
   (lod-camera-position :accessor apds-lod-camera-position :type matrix4)
   (lod-camera-position-object-space :accessor apds-lod-camera-position-object-space :type matrix4)

   (world-matrix-dirty :accessor apds-world-matrix-dirty :type boolean)
   (view-matrix-dirty :accessor apds-view-matrix-dirty :type boolean)
   (proj-matrix-dirty :accessor apds-proj-matrix-dirty :type )boolean
   (world-view-matrix-dirty :accessor apds-world-view-matrix-dirty :type boolean)
   (view-proj-matrix-dirty :accessor apds-view-proj-matrix-dirty :type boolean)
   (world-view-proj-matrix-dirty :accessor apds-world-view-proj-matrix-dirty :type boolean)
   (inverse-world-matrix-dirty :accessor apds-inverse-world-matrix-dirty :type boolean)
   (inverse-world-view-matrix-dirty :accessor apds-inverse-world-view-matrix-dirty :type boolean)
   (inverse-view-matrix-dirty :accessor apds-inverse-view-matrix-dirty :type boolean)
   (inverse-transpose-world-matrix-dirty :accessor apds-inverse-transpose-world-matrix-dirty :type boolean)
   (inverse-transpose-world-view-matrix-dirty :accessor apds-inverse-transpose-world-view-matrix-dirty :type boolean)
   (camera-position-dirty :accessor apds-camera-position-dirty :type boolean)
   (camera-position-object-space-dirty :accessor apds-camera-position-object-space-dirty :type boolean)
   (texture-view-proj-matrix-dirty :accessor apds-texture-view-proj-matrix-dirty :type vector)
   (texture-world-view-proj-matrix-dirty :accessor apds-texture-world-view-proj-matrix-dirty :type vector)
   (spotlight-view-proj-matrix-dirty :accessor apds-spotlight-view-proj-matrix-dirty :type vector)
   (spotlight-world-view-proj-matrix-dirty :accessor apds-spotlight-world-view-proj-matrix-dirty :type vector)
   (shadow-cam-depth-ranges-dirty :accessor apds-shadow-cam-depth-ranges-dirty :type vector)

   (ambient-light :accessor apds-ambient-light :type colour-value)
   (fog-colour :accessor apds-fog-colour :type colour-value)
   (fog-params :accessor apds-fog-params :type vector4)
   (pass-number :accessor apds-pass-number :type (integer 0))
   (scene-depth-range :accessor apds-scene-depth-range :type vector4)
   (scene-depth-range-dirty :accessor apds-scene-depth-range-dirty :type boolean)
   (lod-camera-position-dirty :accessor apds-lod-camera-position-dirty :type boolean)
   (lod-camera-position-object-space-dirty :accessor apds-lod-camera-position-object-space-dirty :type boolean)
   (current-renderable :accessor apds-current-renderable :type renderable)
   (current-camera :accessor apds-current-camera :type camera)
   (camera-relative-rendering :accessor apds-camera-relative-rendering :type boolean)
   (camera-relative-position :accessor apds-camera-relative-position :type vector3)
   (current-light-list :accessor apds-current-light-list :type list)
   (current-texture-projector :accessor apds-current-texture-projector :type vector)
   (current-render-target :accessor apds-current-render-target :type render-target)
   (current-viewport :accessor apds-current-viewport :type viewport)
   (current-scene-manager :accessor apds-current-scene-manager :type scene-manager)
   (main-cam-bounds-info :accessor apds-main-cam-bounds-info :type visible-objects-bounds-info)
   (current-pass :accessor apds-current-pass :type pass)
   (blank-light :accessor apds-blank-light :type light))
  (:documentation
   "his utility class is used to hold the information used to generate the matrices
and other information required to automatically populate GpuProgramParameters.

This class exercises a lazy-update scheme in order to avoid having to update all
the information a GpuProgramParameters class could possibly want all the time. 
It relies on the SceneManager to update it when the base data has changed, and
will calculate concatenated matrices etc only when required, passing back precalculated
matrices when they are requested more than once when the underlying information has
not altered."))

(defgeneric set-world-matrices (apds matrices))
(defgeneric set-current-camera (apds camera use-camera-relative))
(defgeneric set-current-light-list (apds light-list))
(defgeneric set-texture-projector (apds frustum index))
(defgeneric get-light-casts-shadows (apds index))
(defgeneric get-light-diffuse-colour (apds index))
(defgeneric get-light-specular-colour (apds index))
(defgeneric get-light-position (apds index))
(defgeneric get-light-as-vector4 (apds index))
(defgeneric get-light-direction (apds index))
(defgeneric get-light-power-scale (apds index))
(defgeneric get-light-attenuation (apds index))
(defgeneric set-ambient-light-colour (apds colour-value))
(defgeneric get-surface-ambient-colour (apds))
(defgeneric get-surface-diffuse-colour (apds))
(defgeneric get-surface-specular-colour (apds))
(defgeneric get-surface-emissive-colour (apds))
(defgeneric get-surface-shininess (apds))
(defgeneric get-derived-ambient-light-colour (apds))
(defgeneric get-derived-scene-colour (apds))
(defgeneric set-fog (apds fog-mode colour-value exp-density linear-start linear-end))
(defgeneric get-texture-view-proj-matrix (apds index))
(defgeneric get-texture-world-view-proj-matrix (apds index))
(defgeneric get-spotlight-view-proj-matrix (apds index))
(defgeneric get-spotlight-world-view-proj-matrix (apds index))
(defgeneric get-texture-transform-matrix (apds index))
(defgeneric get-texture-size (apds index))
(defgeneric get-inverse-texture-size (apds index))
(defgeneric get-packed-texture-size (apds index))
(defgeneric get-shadow-scene-depth-range (apds index))
(defgeneric get-time (apds))

(defgeneric get-time-0-x (apds x))
(defgeneric get-cos-time-0-x (apds x))
(defgeneric get-sin-time-0-x (apds x))
(defgeneric get-tan-time-0-x (apds x))
(defgeneric get-time-0-x-packed (apds x))
(defgeneric get-time-0-1 (apds x))
(defgeneric get-cos-time-0-1 (apds x))
(defgeneric get-sin-time-0-1 (apds x))
(defgeneric get-tan-time-0-1 (apds x))
(defgeneric get-time-0-1-packed (apds x))
(defgeneric get-time-0-2pi (apds x))
(defgeneric get-cos-time-0-2pi (apds x))
(defgeneric get-sin-time-0-2pi (apds x))
(defgeneric get-tan-time-0-2pi (apds x))
(defgeneric get-time-0-2pi-packed (apds x))
(defgeneric get-frame-time (apds))
(defgeneric get-fps (apds))
(defgeneric get-viewport-witdth (apds))
(defgeneric get-viewport-height (apds))
(defgeneric get-inverse-viewport-witdth (apds))
(defgeneric get-inverse-viewport-height (apds))
(defgeneric get-viewport-direction (apds))
(defgeneric get-viewport-side-vector (apds))
(defgeneric get-viewport-up-vector (apds))
(defgeneric get-fov (apds))
(defgeneric get-near-clip-distance (apds))
(defgeneric get-far-clip-distance (apds))
(defgeneric inc-pass-number (apds))
(defgeneric update-custom-gpu-parameter (apds gpu-program-parameter-auto-constant-entry gpu-program-parameters))
