(in-package :neptune)

(defun draw-circle (renderer x y r)
  "Render a circle of radius R centred at (X,Y) using Bresenham's algorithm"
  (labels ((radius-error (xi yi r)
             (abs (+ (* xi xi) (* yi yi) (- (* r r)))))

           (decrement-x-p (xi yi)
             (plusp (+ (* 2 (+ (* xi xi) (* yi yi) (- (* r r)) (1+ (* 2 yi))))
                       (- 1 (* 2 xi))))))

    (loop for yi from 0 
          for xi = r then (if (decrement-x-p xi yi) (1- xi) xi)
          for points = (mapcar (lambda (p) (list (+ x (first p))
                                                 (+ y (second p))))
                               (list
                                 (list xi yi)
                                 (list yi xi)
                                 (list (- yi) xi)
                                 (list (- xi) yi)
                                 (list (- xi) (- yi))
                                 (list (- yi) (- xi))
                                 (list yi (- xi))
                                 (list xi (- yi))))   
          until (< xi yi) do
          (mapcar (lambda (p)
                    (sdl2:render-draw-point renderer (first p) (second p)))
                  points))))

(defun render-point-vector (renderer point vector &optional (scale 1))
  "Render a VECTOR originating from POINT scaled by a factor of SCALE"
  (let ((vector (vec:scale vector scale)))
    (sdl2:render-draw-line
      renderer
      (floor (vec:vec-x point))
      (floor (vec:vec-y point))
      (floor (+ (vec:vec-x point) (vec:vec-x vector)))
      (floor (+ (vec:vec-y point) (vec:vec-y vector))))))
