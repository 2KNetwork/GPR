;;;;; ukol 1 ;;;;;

(defun is-convex-polygon (list-of-points)
  (let ((result (mapcar (lambda (x y z) (z (cross-product 
                                            (minus (point (x y) (y y) 0)
                                                   (point (x x) (y x) 0))
                                            (minus (point (x z) (y z) 0)
                                                   (point (x z) (y z) 0)))))
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
  (let ((result  (mapcar (lambda (x y) (z (cross-product (minus  (2d-2-3d x)
                                                                 (2d-2-3d point))
                                                         (minus (2d-2-3d y)
                                                                (2d-2-3d point)))))
            polygon-points
            (rotate-list polygon-points))))
    (every (lambda (x y) (eql (< x 0)
                              (< y 0)))
           result
           (rotate-list result))))

;;;;; ukol 3 ;;;;;
        
                                             








       
 
