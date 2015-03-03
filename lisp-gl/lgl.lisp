;; -*- mode: lisp; encoding: utf-8; -*-

;; Body

(defclass point ()
  ((coordinates :reader coordinates)))

(defun point (x-coordinate &rest rest-coordinates)
  (unless (every #'numberp (cons x-coordinate rest-coordinates))
    (error "The coordinates must be numbers."))
  (let ((point (make-instance 'point)))
    (setf (slot-value point 'coordinates) (cons x-coordinate rest-coordinates))
    point))

(defmethod dimension ((point point))
  (length (coordinates point)))

(defmethod x ((point point))
  (first (coordinates point)))

(defmethod y ((point point))
  (unless (>= (dimension point) 2)
    (error "The point must have dimension at least two."))
  (second (coordinates point)))

(defmethod z ((point point))
  (unless (>= (dimension point) 3)
    (error "The point must have dimension at least three."))
  (third (coordinates point)))

(defvar *origin* (point 0 0))

;; Vektory

(defclass vect ()
  ((coordinates :reader coordinates)))

(defun vect (x-coordinate &rest rest-coordinates)
  (unless (every #'numberp (cons x-coordinate rest-coordinates))
    (error "The coordinates must be numbers."))
  (let ((vector (make-instance 'vect)))
    (setf (slot-value vector 'coordinates) (cons x-coordinate rest-coordinates))
    vector))

(defmethod dimension ((vector vect))
  (length (coordinates vector)))

(defmethod x ((vector vect))
  (first (coordinates vector)))

(defmethod y ((vector vect))
  (unless (>= (dimension vector) 2)
    (error "The vector must have dimension at least two."))
  (second (coordinates vector)))

(defmethod z ((vector vect))
  (unless (>= (dimension vector) 3)
    (error "The vector must have dimension at least three."))
  (third (coordinates vector)))

;; Operace s body a vektory

(defmethod minus ((p1 point) (p2 point))
  (unless (= (dimension p1) (dimension p2))
    (error "Points must have the same dimension."))
  (apply #'vect 
         (mapcar #'- 
                 (coordinates p1)
                 (coordinates p2))))

(defmethod plus ((p point) (v vect))
  (unless (= (dimension p) (dimension v))
    (error "The point must have the same dimenstion as the vector."))
  (apply #'point 
         (mapcar #'+ 
                 (coordinates p)
                 (coordinates v))))

(defmethod plus ((v1 vect) (v2 vect))
  (unless (= (dimension v1) (dimension v2))
    (error "Vectors must have the same dimenstion."))
  (apply #'vect 
         (mapcar #'+ 
                 (coordinates v1)
                 (coordinates v2))))

(defmethod mult (coef (v vect))
  (apply #'vect (mapcar (lambda (coord) (* coef coord)) (coordinates v))))

(defmethod phi ((vector vect))
  (unless (= (dimension vector) 2)
    (error "The vector must have dimension two."))
  (let ((x (x vector))
        (y (y vector)))
    (cond
     ((plusp x) (atan (/ y x))) 
     ((minusp x) (+ pi (atan (/ y x)))) 
     (t (* (signum y) (/ pi 2))))))

(defmethod rotate ((vector vect) angle)
  (unless (= (dimension vector) 2)
    (error "The vector must have dimension two."))
  (let ((phi (+ (phi vector) angle))
        (r (sqrt (+ (expt (x vector) 2)
                    (expt (y vector) 2)))))
    (vect (* r (cos phi))
          (* r (sin phi)))))

(defmethod cross-product ((u vect) (v vect))
  (unless (= 3 (dimension u) (dimension v))
    (error "The vectors must have dimension three."))
  (vect (- (* (y u) (z v))
           (* (z u) (y v)))
        (- (* (z u) (x v))
           (* (x u) (z v)))
        (- (* (x u) (y v))
           (* (y u) (x v)))))

;; OpenGL

(defun resize-canvas (canvas x y width height)
  (declare (ignore x y))
  (opengl:rendering-on (canvas)       
    (when (zerop height) (setf height 1))
    (opengl:gl-viewport 0 0 width height)
    (opengl:gl-matrix-mode opengl:*gl-projection*)
    (opengl:gl-load-identity)
    (opengl:gl-ortho 0.0d0 (coerce width 'double-float) 0.0d0 (coerce height 'double-float) -1000.0d0 1000.0d0))
  (funcall
   (slot-value canvas 'capi::display-callback) canvas))


(defmacro define-opengl-interface ((name) &body body)
  (with-unique-names (display-callback-name)
    `(progn
       (capi:define-interface ,name ()
         ()
         (:panes (canvas opengl:opengl-pane
                         :configuration (list :rgba t :depth-buffer 16)
                         :display-callback ',display-callback-name
                         :resize-callback 'resize-canvas
                         :min-width 800
                         :min-height 600))
         (:default-initargs :title "OpenGL interface"))
       (defun ,display-callback-name (canvas &rest args)
         (declare (ignore args))
         (render-on (canvas)
                    ,@body)))))


(defmethod canvas-height ((canvas opengl:opengl-pane))
  (capi::pane-height canvas))

(defmethod canvas-width ((canvas opengl:opengl-pane))
  (capi::pane-width canvas))


(defun display-opengl-interface (name)
  (capi:display (make-instance name)))

(defmacro render-on ((opengl-pane) &body body)
  `(opengl:rendering-on (,opengl-pane)
     (opengl:gl-clear-color 1.0 1.0 1.0 1.0) 
     (opengl:gl-clear opengl:*gl-color-buffer-bit*)
     (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
     (opengl:gl-depth-func opengl:*gl-less*)
     ;(opengl:gl-enable opengl:*gl-depth-test*)
     ,@body
     (opengl:gl-flush)))

(defvar *inside-opengl-block* nil)
(defvar *bases-stack*)
(defvar *base*)
(defvar *vertex-transformation*)

(defmacro opengl-interface (slots (&key (display (lambda (canvas &rest args) (declare (ignore canvas args))))
                                        (mouse-press-primary (lambda (canvas point) (declare (ignore canvas point))))
                                        (mouse-press-secondary (lambda (canvas point) (declare (ignore canvas point))))
                                        (vertex-transformation #'base-vertex-transformation)
                                        (key-press (lambda (canvas char) (declare (ignore canvas char))))))
    (with-unique-names (opengl-canvas opengl-interface)
      `(progn
         (defclass ,opengl-canvas (opengl:opengl-pane)
           ,slots)
         (capi:define-interface ,opengl-interface ()
           ()
           (:panes (canvas ,opengl-canvas
                           :configuration (list :rgba t :depth-buffer 16)
                           :display-callback (lambda  (canvas &rest args)
                                               (declare (ignore args) (ignorable canvas))
                                               (render-on (canvas)
                                                 (let ((*inside-opengl-block* t)
                                                       (*bases-stack* nil)
                                                       (*base* (list (point 0 0) (vect 1 0) (vect 0 1)))
                                                       (*vertex-transformation* ,vertex-transformation))
                                                   (funcall ,display canvas))))
                           :resize-callback 'resize-canvas
                           :input-model `(((:button-1 :press) ,(lambda (canvas x y)
                                                                 (funcall ,mouse-press-primary canvas (point x (- (canvas-height canvas) y)))))
                                          ((:button-3 :press) ,(lambda (canvas x y)
                                                                 (funcall ,mouse-press-secondary canvas (point x (- (canvas-height canvas) y)))))
                                          (:character ,(lambda (canvas x y char)
                                                         (declare (ignore x y))
                                                         (funcall ,key-press canvas char))))
                           :min-width 800
                           :min-height 600))
           (:default-initargs :title "OpenGL interface"))
         (handler-case
             (display-opengl-interface ',opengl-interface)
           (error () nil)))))

(defmacro opengl (&body body)
  `(opengl-interface 
    ()
    (:display (lambda (canvas)
                (declare (ignorable canvas))
                ,@body))))


(defmethod invalidate ((canvas opengl:opengl-pane))
  (capi:apply-in-pane-process canvas #'gp:invalidate-rectangle canvas))
          

(defun color (point)
  (unless *inside-opengl-block*
    (error "The function color must be called inside opengl macros."))
  (unless (= (dimension point) 3)
    (error "The point has to have dimension three."))
  (unless (every (lambda (c) (<= 0 c 1)) (coordinates point))
    (error "Each coordinate of the point must be between 0 and 1."))
  (opengl:gl-color3-d (coerce (x point) 'double-float)
                      (coerce (y point) 'double-float)
                      (coerce (z point) 'double-float)))

(defun color-point (color)
  (apply #'point (rest (map 'list #'identity (color:get-color-spec color)))))
     

(defvar *inside-polygon-block* nil)

(defmacro polygon (&body body)
  `(progn
     (unless *inside-opengl-block*
       (error "Macro polygon must be used inside opengl macros."))
     (opengl:gl-begin opengl:*gl-polygon*)
     (let ((*inside-polygon-block* t))
       ,@body)
     (opengl:gl-end)))

(defmethod vertex ((vertex point))
  (unless *inside-polygon-block*
    (error "The function must be called inside macro polygon."))
  (let ((dim (dimension vertex)))
    (unless (<= 2 dim 3)
      (error "The point must have dimension two or three."))
    (if (= dim 2)
        (let ((vertex-t (funcall *vertex-transformation* vertex)))
          (opengl:gl-vertex2-d (coerce (x vertex-t) 'double-float) (coerce (y vertex-t) 'double-float)))
      (opengl:gl-vertex3-d (coerce (x vertex) 'double-float) (coerce (y vertex) 'double-float) (coerce (z vertex) 'double-float)))))

(defun get-color ()
  (let ((vect (opengl:make-gl-vector :float 4)))
    (opengl:gl-get-floatv opengl:*gl-current-color* vect)
    (point (opengl:gl-vector-aref vect 0)
           (opengl:gl-vector-aref vect 1)
           (opengl:gl-vector-aref vect 2))))


(defmethod base-vertex-transformation ((point point))
  (plus (first *base*)
        (plus (mult (x point) (second *base*))
              (mult (y point) (third *base*)))))

(defmethod c-s-translate ((vector vect))
  (when *inside-polygon-block*
    (error "The function can not be used inside macro polygon."))
  (setf (first *base*) (plus (first *base*) 
                             (plus (mult (x vector) (second *base*))
                                   (mult (y vector) (third *base*))))))

(defun c-s-rotate (angle)
  (when *inside-polygon-block*
    (error "The function can not be used inside macro polygon."))
  (setf (second *base*) (rotate (second *base*) angle)
        (third *base*) (rotate (third *base*) angle)))

(defun c-s-scale (x-coef y-coef)
  (when *inside-polygon-block*
    (error "The function can not be used inside macro polygon."))
  (setf (second *base*) (mult x-coef (second *base*))
        (third *base*) (mult y-coef (third *base*))))

(defun base-push ()
  (push (copy-list *base*) *bases-stack*))

(defun base-pop ()
  (setf *base* (pop *bases-stack*)))

(defmacro with-c-s-pushed (&body body)
  `(progn
     (base-push)
     (unwind-protect 
         (progn
           ,@body)
       (base-pop))))
     




