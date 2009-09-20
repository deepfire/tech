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
;;;; OgreAnimable.h
;;;;
(defclass animable-value ()
  ((type :accessor animable-value-type :type (member :int :real :vector2 :vector3 :vector4 :quaternion :color :radian :degree))
   (value :accessor animable-value))
  (:documentation
   "Animable properties are those which can be altered over time by a 
predefined keyframe sequence. They may be set directly, or they may
be modified from their existing state (common if multiple animations
are expected to apply at once). Implementors of this interface are
expected to override the 'setValue', 'setCurrentStateAsBaseValue' and 
'applyDeltaValue' methods appropriate to the type in question, and to 
initialise the type.

AnimableValue instances are accessible through any class which extends
AnimableObject in order to expose it's animable properties.

This class is an instance of the Adapter pattern, since it generalises
access to a particular property. Whilst it could have been templated
such that the type which was being referenced was compiled in, this would
make it more difficult to aggregated generically, and since animations
are often comprised of multiple properties it helps to be able to deal
with all values through a single class."))

(defgeneric set-as-base-value (animable-value value))
(defgeneric set-current-state-as-base-value (animable-value))
(defgeneric set-value (animable-value value))
(defgeneric reset-base-value (animable-value))
(defgeneric apply-delta-value (animable-value delta-value))

(defclass animable-object ()
  ((animable-dictionary :type hash-table :docstring "Static map of class name to list of animable value names."))
  (:documentation
   "Defines an interface to classes which have one or more AnimableValue
instances to expose."))

(defgeneric create-animable-dictionary (animable))
(defgeneric initialize-animable-dictionary (animable))
(defgeneric get-animable-dictionary-name (animable)
  (:documentation
   "Get the name of the animable dictionary for this class."))
(defun %get-animable-value-names (animable))
(defgeneric get-animable-value-names (animable))
(defgeneric create-animable-value (animable value-name))

(defvar *default-interpolation-mode* :linear)
(defvar *default-rotation-interpolation-mode* :linear)

(define-execute-with-special default-interpolation-mode)
(define-execute-with-special default-rotation-interpolation-mode)

;;;;
;;;; OgreAnimation.h
;;;;
(defclass animation ()
  ((name :accessor animation-name)
   (length :accessor animation-length :type real)

   (interpolation-mode :accessor animation-interpolation-mode :type (member :linear :spline))
   (rotation-interpolation-mode :accessor animation-rotation-interpolation-mode :type (member :linear :spherical))

   (node-tracks :accessor animation-node-tracks :initform (make-hash-table) :type hash-table)
   (numeric-tracks :accessor animation-numeric-tracks :initform (make-hash-table) :type hash-table)
   (vertex-tracks :accessor animation-vertex-tracks :initform (make-hash-table) :type hash-table)

   (keyframe-times :accessor animation-keyframe-times :initform nil :type list)
   (keyframe-times-dirty-p :accessor animation-keyframe-times-dirty-p :initform nil :type boolean))
  (:documentation
   "This class defines the interface for a sequence of animation, whether that
be animation of a mesh, a path along a spline, or possibly more than one
type of animation in one. An animation is made up of many 'tracks', which are
the more specific types of animation.

You should not create these animations directly. They will be created via a parent
object which owns the animation, e.g. Skeleton."))

(defgeneric create-node-track (animation handle &optional node))
(defgeneric create-numeric-track (animation handle &optional animable-value))
(defgeneric create-vertex-track (animation handle vertex-animation-type &optional vertex-data))

(defgeneric get-num-node-tracks (animation))
(defgeneric get-num-numeric-tracks (animation))
(defgeneric get-num-vertex-tracks (animation))

(defgeneric get-node-track (animation handle))
(defgeneric get-numeric-track (animation handle))
(defgeneric get-vertex-track (animation handle))

(defgeneric has-node-track (animation handle))
(defgeneric has-numeric-track (animation handle))
(defgeneric has-vertex-track (animation handle))

(defgeneric destroy-node-track (animation handle))
(defgeneric destroy-numeric-track (animation handle))
(defgeneric destroy-vertex-track (animation handle))

(defgeneric destroy-all-node-tracks (animation))
(defgeneric destroy-all-numeric-tracks (animation))
(defgeneric destroy-all-vertex-tracks (animation))

(defgeneric destroy-all-tracks (animation))

(defgeneric animation-apply (animation target time-position weight scale &key &allow-other-keys))

;; let's see how iterators are used and then redo this in a more sensible manner 
(defgeneric animation-node-tracks (animation))
(defgeneric animation-numeric-tracks (animation))
(defgeneric animation-vertex-tracks (animation))

(defgeneric animation-optimise (animation &optional discard-identity-node-tracks)
  (:documentation
   "When you export an animation, it is possible that certain tracks
have been keyframed but actually don't include anything useful - the
keyframes include no transformation. These tracks can be completely
eliminated from the animation and thus speed up the animation. 
In addition, if several keyframes in a row have the same value, 
then they are just adding overhead and can be removed.

Since track-less and identity track has difference behavior for
accumulate animation blending if corresponding track presenting at
other animation that is non-identity, and in normally this method
didn't known about the situation of other animation, it can't deciding
whether or not discards identity tracks. So there have a parameter
allow you choose what you want, in case you aren't sure how to do that,
you should use Skeleton::optimiseAllAnimations instead."))
(defgeneric animation-optimise-node-tracks (animation &optional discard-identity-tracks))
(defgeneric animation-optimise-vertex-tracks (animation))

(defgeneric animation-collect-identity-node-tracks (animation tracks))
(defgeneric animation-destroy-node-tracks (animation tracks))

(defgeneric animation-clone (animation new-animation-name))

(defgeneric animation-key-frame-list-changed-p (animation))

(defgeneric animation-get-time-index (animation timepos))

(defgeneric animation-build-keyframe-time-list (animation))

;;;;
;;;; OgreAnimationState.h
;;;;
(defclass animation-state ()
  ((animation-name :accessor animation-state-animation-name)
   (parent-state-set :accessor animation-state-parent-state-set)
   
   (time-position :accessor animation-state-time-position)
   (length :accessor animation-state-length)
   (weight :accessor animation-state-weight)

   (enabled-p :accessor animation-state-enabled-p)
   (looping-p :accessor animation-state-looping-p)
   (blend-mask :accessor animation-blend-mask))
  (:documentation
   "Represents the state of an animation and the weight of it's influence. 
Other classes can hold instances of this class to store the state of any animations
they are using."))

(defgeneric animation-state-add-time (animation-state delta)
  (:documentation
   "Proposed name: animation-state-advance."))

(defgeneric animation-state-ended-p (animation-state)
  (:documentation
   "Returns true if the animation has reached the end and is not looping."))

(defgeneric animation-state-equalp (animation-state-1 animation-state-2))
(defgeneric copy-animation-state (animation-state))

(defgeneric animation-state-create-blend-mask (animation-state blend-mask-size-hint &optional initial-weight)
  (:documentation
   "Create a new blend mask with the given number of entries.
In addition to assigning a single weight value to a skeletal animation,
it may be desirable to assign animation weights per bone using a 'blend mask'."))

(defgeneric animation-state-destroy-blend-mask (animation-state))

(defgeneric animation-state-blend-mask-data (animation-state))

(defgeneric animation-state-blend-mask-entry (animation-state handle))
(defgeneric (setf animation-state-blend-mask-entry) (weight animation-state handle))

(defclass animation-state-set ()
  (;; OGRE_AUTO_MUTEX
   (dirty-frame-number :accessor animation-state-set-dirty-frame-number)
   (animation-states :accessor animation-state-set-states :initform (make-hash-table) :type hash-table)
   (enabled-animation-states :accessor animation-state-set-enabled-states :initform nil :type list)))

(defgeneric copy-animation-state-set (animation-state-set))
(defgeneric animation-state-set-create-animation-state (animation-state-set animation-name timepos length &otional weight enabled-p))
(defgeneric animation-state-set-animation-state (animation-state-set animation-name))
(defgeneric animation-state-set-remove (animation-state-set animation-name))
(defgeneric animation-state-set-remove-all (animation-state-set))
(defgeneric animation-state-set-copy-matching (animation-state-set target-animation-state-set))
(defgeneric animation-state-set-notify-dirty (animation-state-set))
(defgeneric animation-state-set-notify-animation-state-enabled (animation-state-set animation-state enabled))
(defgeneric animation-state-set-has-enabled-state (animation-state-set))

;;;;
;;;; OgreAnimationTrack.h
;;;;
(defclass time-index ()
  ((timepos :accessor time-index-timepos :type real)
   (keyindex :accessor :time-index-keyindex :type (integer 0)))
  (:documentation
   "Time index object used to search keyframe at the given position."))

(defgeneric time-index-has-keyindex-p (time-index))

(defclass animation-track-listener ()
  ()
  (:documentation
   "Listener allowing you to override certain behaviour of a track, 
for example to drive animation procedurally."))

(defgeneric get-interpolated-keyframe (animation-track-listener animation-track time-index keyframe))

(defclass animation-track ()
  ((animation :accessor animation-track-animation)
   (handle :accessor animation-track-handle)
   (keyframes :accessor animation-track-keyframes :type vector)
   (listener :accessor animation-track-listener)
   (keyframe-index-map :accessor animation-track-keyframe-index-map :type vector))
  (:documentation
   "A 'track' in an animation sequence, i.e. a sequence of keyframes which affect a
certain type of animable object.

This class is intended as a base for more complete classes which will actually
animate specific types of object, e.g. a bone in a skeleton to affect
skeletal animation. An animation will likely include multiple tracks each of which
can be made up of many KeyFrame instances. Note that the use of tracks allows each animable
object to have it's own number of keyframes, i.e. you do not have to have the
maximum number of keyframes for all animable objects just to cope with the most
animated one.

Since the most common animable object is a Node, there are options in this class for associating
the track with a Node which will receive keyframe updates automatically when the 'apply' method
is called.

By default rotation is done using shortest-path algorithm.
It is possible to change this behaviour using
setUseShortestRotationPath() method."))

(defgeneric animation-track-keyframe (animation-track index))
(defgeneric num-animation-track-keyframes (animation-track))
(defgeneric animation-track-keyframes-at-time (animation-track time-index &optional first-key-index)
  (:documentation
   "Gets the 2 KeyFrame objects which are active at the time given, and the blend value between them.

At any point in time  in an animation, there are either 1 or 2 keyframes which are 'active',
1 if the time index is exactly on a keyframe, 2 at all other times i.e. the keyframe before
and the keyframe after.

This method returns those keyframes given a time index, and also returns a parametric
value indicating the value of 't' representing where the time index falls between them.
E.g. if it returns 0, the time index is exactly on keyFrame1, if it returns 0.5 it is
half way between keyFrame1 and keyFrame2 etc."))

(defgeneric animation-track-do-create-keyframe (animation-track time))
(defgeneric animation-track-create-keyframe (animation-track timepos))
(defgeneric animation-track-remove-keyframe (animation-track index))
(defgeneric animation-track-remove-all-keyframes (animation-track))

(defgeneric get-interpolated-keyframe (animation-track time-index keyframe)
  (:documentation
   "Gets a KeyFrame object which contains the interpolated transforms at the time index specified.

The KeyFrame objects held by this class are transformation snapshots at 
discrete points in time. Normally however, you want to interpolate between these
keyframes to produce smooth movement, and this method allows you to do this easily.
In animation terminology this is called 'tweening'."))

(defgeneric animation-track-keyframe-data-changed-p (animation-track))
(defgeneric animation-track-has-nonzero-keyframes-p (animation-track))
(defgeneric animation-track-optimize (animation-track))
(defgeneric animation-track-collect-keyframe-times (animation-track))
(defgeneric animation-track-build-keyframe-index-map (animation-track))

(defgeneric clone-animation-track (animation-track))
(defgeneric animation-track-populate-clone (animation-track clone))

(defclass numeric-animation-track (animation-track)
  ((animable-value :accessor numeric-animation-track-animable-value)))

(defgeneric animation-track-create-numeric-keyframe (animation-track timepos)
  (:documentation
   "Creates a new KeyFrame and adds it to this animation at the given time index.

It is better to create KeyFrames in time order. Creating them out of order can result 
in expensive reordering processing. Note that a KeyFrame at time index 0.0 is always created
for you, so you don't need to create this one, just access it using getKeyFrame(0)"))

(defmethod animation-track-create-keyframe ((o numeric-animation-track) timepos))
(defmethod get-interpolated-keyframe ((o numeric-animation-track) time-index keyframe))
(defmethod animation-track-keyframe ((o numeric-animation-track) index))
(defmethod animation-apply ((o numeric-animation-track) (self null) time-index &optional  (weight 1.0) (scale 1.0) &key &allow-other-keys))
(defmethod animation-apply ((o numeric-animation-track) (a animable-value) time-index &optional  (weight 1.0) (scale 1.0) &key &allow-other-keys))
(defmethod animation-track-do-create-keyframe ((o numeric-animation-track) time))

(defclass node-animation-splines ()
  ((position-spline)
   (scale-spline)
   (rotation-spline)))

(defclass node-animation-track (animation-track)
  ((node :accessor numeric-animation-track-node)
   (splines :accessor node-animation-splines :documentation "Allocate on demand for better memory footprint.")
   (spline-build-needed :accessor node-animation-spline-build-needed)
   (use-shortest-rotation-path :accessor node-animation-track-use-shortest-rotation-path-p :initform nil :type boolean)))

(defmethod animation-track-create-keyframe ((o node-animation-track) timepos))
(defmethod get-interpolated-keyframe ((o node-animation-track) time-index keyframe))
(defmethod animation-track-keyframe ((o node-animation-track) index))
(defmethod animation-apply ((o node-animation-track) (self null) time-index &optional  (weight 1.0) (scale 1.0) &key &allow-other-keys))
(defmethod animation-apply ((o node-animation-track) (a node) time-index &optional  (weight 1.0) (scale 1.0) &key &allow-other-keys))

(defgeneric node-animation-track-build-interpolation-splines (node-animation-track))

(defclass vertex-animation-track (animation-track)
  ((target-mode :accessor vertex-animation-track-target-mode :type (member :hardware :software))
   (vertex-data :accessor vertex-animation-track-vertex-data))
  (:documentation
   "Vertex animation comes in 2 types, morph and pose. The reason
for the 2 types is that we have 2 different potential goals - to encapsulate
a complete, flowing morph animation with multiple keyframes (a typical animation,
but implemented by having snapshots of the vertex data at each keyframe), 
or to represent a single pose change, for example a facial expression. 
Whilst both could in fact be implemented using the same system, we choose
to separate them since the requirements and limitations of each are quite
different.

Morph animation is a simple approach where we have a whole series of 
snapshots of vertex data which must be interpolated, e.g. a running 
animation implemented as morph targets. Because this is based on simple
snapshots, it's quite fast to use when animating an entire mesh because 
it's a simple linear change between keyframes. However, this simplistic 
approach does not support blending between multiple morph animations. 
If you need animation blending, you are advised to use skeletal animation
for full-mesh animation, and pose animation for animation of subsets of 
meshes or where skeletal animation doesn't fit - for example facial animation.
For animating in a vertex shader, morph animation is quite simple and 
just requires the 2 vertex buffers (one the original position buffer) 
of absolute position data, and an interpolation factor. Each track in 
a morph animation refrences a unique set of vertex data.

Pose animation is more complex. Like morph animation each track references
a single unique set of vertex data, but unlike morph animation, each 
keyframe references 1 or more 'poses', each with an influence level. 
A pose is a series of offsets to the base vertex data, and may be sparse - ie it
may not reference every vertex. Because they're offsets, they can be 
blended - both within a track and between animations. This set of features
is very well suited to facial animation.

For example, let's say you modelled a face (one set of vertex data), and 
defined a set of poses which represented the various phonetic positions 
of the face. You could then define an animation called 'SayHello', containing
a single track which referenced the face vertex data, and which included 
a series of keyframes, each of which referenced one or more of the facial 
positions at different influence levels - the combination of which over
time made the face form the shapes required to say the word 'hello'. Since
the poses are only stored once, but can be referenced may times in 
many animations, this is a very powerful way to build up a speech system.

The downside of pose animation is that it can be more difficult to set up.
Also, since it uses more buffers (one for the base data, and one for each
active pose), if you're animating in hardware using vertex shaders you need
to keep an eye on how many poses you're blending at once. You define a
maximum supported number in your vertex program definition, see the 
includes_pose_animation material script entry. 

So, by partitioning the vertex animation approaches into 2, we keep the
simple morph technique easy to use, whilst still allowing all 
the powerful techniques to be used. Note that morph animation cannot
be blended with other types of vertex animation (pose animation or other
morph animation); pose animation can be blended with other pose animation
though, and both types can be combined with skeletal animation. Also note
that all morph animation can be expressed as pose animation, but not vice
versa."))

(defmethod get-interpolated-keyframe ((o vertex-animation-track) time-index keyframe))
(defmethod animation-apply ((o vertex-animation-track) (self null) time-index &optional  (weight 1.0) (scale 1.0) &key &allow-other-keys))
(defmethod animation-apply ((o vertex-animation-track) (a vertex-data) time-index &optional  (weight 1.0) (scale 1.0) &key &allow-other-keys))

(defclass morph-animation-track (vertex-animation-track)
  ())

(defmethod animation-track-create-keyframe ((o morph-animation-track) timepos))
(defmethod animation-track-keyframe ((o morph-animation-track) index))

(defclass pose-animation-track (vertex-animation-track)
  ())

(defmethod animation-track-create-keyframe ((o pose-animation-track) timepos))
(defmethod animation-track-keyframe ((o pose-animation-track) index))

(defgeneric apply-pose-to-vertex-data (pose-animation-track pose vertex-data influence))

