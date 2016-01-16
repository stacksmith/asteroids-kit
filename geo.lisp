;; asteroids
;; 
; Controls: 
;; Rotate a f   Thrust j  Fire <spacebar>
;;
;; To run from commandline: sbcl --load asteroids.lisp --eval "(asteroids:main)"
;;
;; In emacs, evaluate the 3 ql:quickload forms with C-c C-c on each line;
;; compile entire file with C-c C-k
;; in REPL, enter (asteroids:main) or do (in-package asteroids), then (main).
;;
(in-package :asteroids-kit)

(defconstant +2pi+ (coerce (* 2 pi) 'single-float) )
(defconstant +pi+ (coerce pi 'single-float) )

(defparameter *screen-width* 800.0)
(defparameter *screen-height* 600.0)
(defparameter *thrust-factor* 0.005)

(defparameter *friction* 0.995)
(defvar *ticks* 0)


(defparameter *window* nil)

(defvar *ship* nil)
(defparameter *lr-map* 0)
(defparameter *is-thrusting* nil)


;;
;; Used for building radial objects at origin.  Generally, template objects.
;; Each radial point is (angle radmul).  Angle is in radians; radmul is multipled by radius.
(defun radial-object (r radpoints)
"from a list of radial points, create a list of points"
  (map 'list
       (lambda (rpt)
	 (let ((rad (* r (second rpt)))
	       (angle (first rpt)))
	   (vector  (* rad (sin angle))  
		    (* rad (cos angle)))))
       radpoints))



(defmacro progx (&body body) (declare (ignore body)))

;;-------------------------------------------------------------------
;; SDL-POLY
;;
;; An obj is a basic drawable vector polygon
;; - a template, at (0 0) position, a list of point pairs
;; - arrays of x and y coordinates for SDL
;; - point count

(defclass sdl-poly ()
  ((template :initform nil :accessor template-of)
   (impl-x :accessor impl-x-of)
   (impl-y :accessor impl-y-of)
   (point-cnt :accessor point-cnt-of)))

;; 
(defun map-sdl-poly (sdlpoly pos angle)
"Map an sdl-poly object by translating and rotating its template. Store results into cffi-array"
  (let ((c (cos angle))
	(s (sin angle)))
    (loop for point in (template-of sdlpoly) 
       for i from 0 to (1- (point-cnt-of sdlpoly)) do
	 (let ((xpt (x point))
	       (ypt (y point)))
	   (progn (setf (cffi:mem-aref (impl-x-of sdlpoly) :short i) 
			(cast-to-int (+ (x pos) (- (* c xpt) (* s ypt))))
			(cffi:mem-aref (impl-y-of sdlpoly) :short i)
			(cast-to-int (+ (y pos) (+ (* s xpt) (* c ypt)))) )
	
		#+nil  (progx (format t "map-ship #~A   ~A ~A~%" i 
				 (cffi:mem-aref (impl-x-of sdlpoly) :short i)
				 (cffi:mem-aref (impl-y-of sdlpoly) :short i) ))
		  )))))


;;-------------------------------------------------------------------
;; O B J
;;
;; An base class for a positioned object:
;; - a position '(x y)
;; - a velocity

(defclass obj (sdl-poly)
  ((pos      :type simple-vector  :initform (vector (/ *screen-width* 2) (/ *screen-height* 2))
	     :initarg :pos  :accessor pos-of)
   (velocity :initform (vector 0.0 0.0) :accessor velocity-of) ))


(defmethod update ((it obj) ticks)
  (let ((pos (pos-of it))
	(velo (velocity-of it)))
    ;;adjust position based on velocity and ticks
    (setf (x pos) 
	  (mod (+ (x pos) (* ticks (x velo) ))
	       *screen-width*))
    (setf (y pos) 
	  (mod (+ (y pos) (* ticks (y velo) ))
	       *screen-height*))))

;;-------------------------------------------------------------------
;; S H I P
;;
;;
(defclass ship (obj)
  ((radius   :initform 0.0 :initarg :radius :accessor radius-of)
   (direction :initform 0.0 :initarg :direction :accessor direction-of)
   (thrust   :initform (vector 0.0 0.0) :accessor thrust-of)
   ))

;; Immediately create the template for the ship's outline.
(defmethod initialize-instance :after ((ship ship) &key)
  (setf (radius-of ship) 12.0
	(template-of ship) (radial-object 12.0 '(( 2.45 1.0) (0.0 1.0) (-2.45 1.0) (-2.5 0.5) (2.5 0.5))))
  ;; Prepare internal arrays for sdl rendering
  (setf 
   (point-cnt-of ship) (length (template-of ship))
   (impl-x-of ship) 
   (cffi:foreign-alloc :short 
		       :initial-contents (lispbuilder-sdl::return-list-for-array (template-of ship) :x))
   (impl-y-of ship) 
   (cffi:foreign-alloc :short 
			    :initial-contents (lispbuilder-sdl::return-list-for-array (template-of ship) :y)))
  ;;TODO: cffi:foreign-free the arrays!
)



(defmethod update :around ((ship ship) ticks)
  (let ((velo (velocity-of ship))
	(dir (direction-of ship))
	(thrust (thrust-of ship)))
    ;; Adjust rotation
    (setf (direction-of ship) 
	  (mod (+ dir (* 0.005 ticks (elt '(0 -1 1 0) *lr-map*)))
	       +2pi+))
    ;; Adjust thrust
    (with-accessors ((xt x) (yt y)) thrust
      (if *is-thrusting*
	  (setf xt (- 0.0 (* *thrust-factor* (sin dir)))
		yt (* *thrust-factor* (cos dir)))
	  (setf xt 0.0
		yt 0.0))
      ;; Adjust velocity
      (with-accessors ((xv x) (yv y)) velo
	(setf xv (* *friction* (+ xt xv))
	      yv (* *friction* (+ yt yv)))))
    (call-next-method) ;;takes care of position
    ;; Prepare points for collision detection and rendering 
    
    (map-sdl-poly ship (pos-of ship) dir)
    ;; (print (points-of ship))
    )
  )

;;
(defmethod renderx ((sdl-poly sdl-poly))
  ;(print (pos-of ship))
  ;;(draw-polygon (points-of ship) :color *green*) ;this is the normal way to draw polygon,but
    ;;(lispbuilder-sdl::gfx-draw-polygon (points-of ship) :surface *default-display* :color *green* :aa nil)
  (cffi:foreign-funcall-pointer (cffi:foreign-symbol-pointer "polygonColor" :library 'sdl-cffi::sdl-gfx)
					 ()
					 lispbuilder-sdl::surface-pointer *default-display*
					 :pointer (impl-x-of sdl-poly)
					 :pointer (impl-y-of sdl-poly)
					 :uint32 (point-cnt-of sdl-poly)
					 lispbuilder-sdl::return-packed-color *green*
					 :int) 
  )


(defun update-all ()
  (let* ((oldticks (shiftf *ticks* (sdl-get-ticks)))
	 (ticks (- *ticks* oldticks)))
    (update *ship* ticks )
    ;(print ticks)

    )
)

(defun main ()
  (with-init (sdl-init-video )
    (setf *window*
	  (window *screen-width* *screen-height*
		  :title-caption "asteroids construction kit"
		  :icon-caption "asteroids" 
		  :fps (make-instance 'fps-fixed :target-frame-rate 60 )   ))
    (sdl-gfx:initialise-default-font sdl-gfx:*font-9x18*)
					;(format t "initialized...")
    ;(setf (frame-rate) 60)
					;(clear-display *black*)
    					; (setf *sound* (make-instance 'sound))
					; (initialize *sound*)
    
    (setf *ship* (make-instance 'ship))
    (print (pos-of *ship*))
    (with-events ()
      (:quit-event () t)		;(shut-down *sound*)
      
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-escape (push-quit-event))
			 (:sdl-key-a (setf *lr-map* (logior 1 *lr-map*)))		
			 (:sdl-key-f (setf *lr-map* (logior 2 *lr-map*)))
			 (:sdl-key-j (setf *is-thrusting* t) )
			 ;;(:sdl-key-space (print (points-of *ship*)))
			 )
		       )

      (:key-up-event (:key key) 
		     (case key
		       (:sdl-key-a (setf *lr-map* (logand 2 *lr-map*)))		
		       (:sdl-key-f (setf *lr-map* (logand 1 *lr-map*)))
		       (:sdl-key-j (setf *is-thrusting* nil) )))
      
					;(:key-up-event (:key key) (key-processor world key :down nil))
      (:mouse-button-down-event (:x x :y y)
				(format t "mouse:(~d,~d)" x y)
				#+nil(if (pt-in-triangle
					  (triangle-p1 *q*)
					  (triangle-p2 *q*)
					  (triangle-p3 *q*)
					  (vector x y))
					 (format t "---HIT---"))
				)
      (:idle () 
	     ;;	       (print (sdl-get-ticks))

	     (clear-display *black*)
	     (update-all)
	     
	     (renderx *ship*)
	     (update-display)
	     )
      )))



;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; T E S T
;; Invoke (test) for entire test.
;; For interactive development, feel free to test-init, mess around, 
;; call test-main, exit to REPL with escape, work some more...
;; at the end, call test-uninit... 

(defun test-init ()
  "initialize systems and open window"
  (init-sdl :flags 'nil)
  (sdl:window 800 600 :title-caption "Asteroids-kit" :icon-caption "Asteroids-kit")
  (setf (sdl:frame-rate) 30)
  (setf *ship* (make-instance 'ship))

)

(defun test-work ()
  "do something to fill the screen"
    (update-display)
)
(defun test-main ()
  "main loop - process events"
  (with-events ()
      (:quit-event () t)		;(shut-down *sound*)
      
      (:key-down-event (:key key)
		       (case key
			 (:sdl-key-escape (push-quit-event))
			 (:sdl-key-a (setf *lr-map* (logior 1 *lr-map*)))		
			 (:sdl-key-f (setf *lr-map* (logior 2 *lr-map*)))
			 (:sdl-key-j (setf *is-thrusting* t) )
			 ;;(:sdl-key-space (print (points-of *ship*)))
			 )
		       )

      (:key-up-event (:key key) 
		     (case key
		       (:sdl-key-a (setf *lr-map* (logand 2 *lr-map*)))		
		       (:sdl-key-f (setf *lr-map* (logand 1 *lr-map*)))
		       (:sdl-key-j (setf *is-thrusting* nil) )))
      
					;(:key-up-event (:key key) (key-processor world key :down nil))
      (:mouse-button-down-event (:x x :y y)
				(format t "mouse:(~d,~d)" x y)
				#+nil(if (pt-in-triangle
					  (triangle-p1 *q*)
					  (triangle-p2 *q*)
					  (triangle-p3 *q*)
					  (vector x y))
					 (format t "---HIT---"))
				)
      (:idle ()
	     (update-swank)
	     ;;	       (print (sdl-get-ticks))

	     (clear-display *black*)
	     (update-all)
	     
	     (renderx *ship*)
	     (update-display)
	     )
      )

    )
(defun test-uninit ()
  "uninit and close window"
  (sdl:push-quit-event)
  (close-audio)
  (quit-sdl :flags 'nil))

(defun test ()
  (test-init)
  (test-work)
  (test-main)
  (test-uninit))
