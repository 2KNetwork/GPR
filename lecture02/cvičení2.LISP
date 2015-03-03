;;;;; ukol 1 ;;;;;
(defun is-convex-polygon (list-of-points)
  (let ((result (mapcar (lambda (x y z) (z (cross-product
                                            (minus (2d-2-3d y)
                                                   (2d-2-3d x))
                                            (minus (2d-2-3d z)
                                                   (2d-2-3d y)))))
                        list-of-points
                        (rotate-list list-of-points)
                        (rotate-list (rotate-list list-of-points)))))
    (every (lambda (x y) (eql (< x 0)
                              (< y 0)))
           result
           (rotate-list result))))
                          
(defun rotate-list (list)
  (append (cdr list) (list (car list))))

;;;;; ukol 2 ;;;;;
(defun 2d-2-3d (point)
  (point (x point) (y point) 0))

(defun is-in-convex-polygon (polygon-points point)
  (let ((result (mapcar (lambda (x y) (z (cross-product (minus (2d-2-3d x)
                                                               (2d-2-3d point))
                                                        (minus (2d-2-3d y)
                                                               (2d-2-3d point)))))
                        polygon-points
                        (rotate-list polygon-points))))
    (every (lambda (x y) (eql (< 0 x)
                              (< 0 y)))
           result
           (rotate-list result))))

;;;;; ukol 3 ;;;;;
(defun draw-window () 
  (opengl-interface
   ((color :initform :red)
    (points :initform nil))
   (:display
    (lambda (canvas)
      (with-slots (color points) canvas
        (color (color-point color))
        (polygon
          (dolist (p points)
            (vertex p)))))
   :mouse-press-primary
   (lambda (canvas point)
     (with-slots (points) canvas
       (if (is-convex-polygon (append points (list point)))
           (setf points (append points (list point)))
         (capi:display-message "You just made me not convex BRO!"))
       (invalidate canvas)))
   :mouse-press-secondary
   (lambda (canvas point)
     (with-slots (color points) canvas
       (if (is-in-convex-polygon points point)
           (if (eql color :red)
               (setf color :blue)
             (setf color :red)))
       (invalidate canvas))))))
        
                                             








       
 
