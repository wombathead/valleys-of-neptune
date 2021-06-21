(ql:quickload :sdl2)
(ql:quickload :sdl2-ttf)

; TODO: replace 'ship' with 'ship'

(load "linalg.lisp")
(load "ship.lisp")
(load "planet.lisp")

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

(defparameter *ship* (make-ship :name "cherokee mist"
                                :pos #(200 200)
                                :thrust 0.05
                                :size 5))

(defparameter *earth* (make-planet :name "earth"
                                   :pos #(400 300)
                                   :mass 10000
                                   :radius 100))

(defparameter *moon* (make-planet :name "moon"
                                  :pos #(600 300)
                                  :vel #(0 0.7)
                                  :mass 10
                                  :radius 10))

(defparameter *planets* (list *earth* *moon*))

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

(defun update ()
  (update-ship *ship*)
  (update-planet-position *earth* *planets*)
  (update-planet-position *moon* *planets*))

(defun render-planet (renderer planet)
  (draw-circle renderer
               (floor (planet-pos-x planet))
               (floor (planet-pos-y planet))
               (planet-radius planet)))

(defun render-direction-vector (renderer ship)
  (let ((dir (polar-to-cart 1 (ship-heading ship))))
    (sdl2:render-draw-line
      renderer
      (floor (ship-pos-x ship))
      (floor (ship-pos-y ship))
      (floor (+ (ship-pos-x ship) (* 10 (vec-x dir))))
      (floor (+ (ship-pos-y ship) (* 10 (vec-y dir)))))))

(defun render-velocity-vector (renderer ship)
  (sdl2:render-draw-line
    renderer
    (floor (ship-pos-x ship))
    (floor (ship-pos-y ship))
    (floor (+ (ship-pos-x ship) (* 10 (ship-vel-x ship))))
    (floor (+ (ship-pos-y ship) (* 10 (ship-vel-y ship))))))

(defun render-ship (renderer ship)
  (format t "Heading: ~A~%" (polar-to-cart 1 (ship-heading ship)))
  (draw-circle renderer
               (floor (ship-pos-x ship))
               (floor (ship-pos-y ship))
               (ship-size ship))
  (render-direction-vector renderer ship)
  (render-velocity-vector renderer ship))

(defun render (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)

  (sdl2:set-render-draw-color renderer 0 255 0 255)

  (mapcar (lambda (p) (render-planet renderer p)) *planets*)

  (render-ship renderer *ship*)

  (sdl2:render-present renderer))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2-ttf:init)
    (sdl2:with-window (window :title "valleys of neptune"
                              :w *screen-width*
                              :h *screen-height*)

      (sdl2:with-renderer (renderer window)

        (sdl2:with-event-loop (:method :poll)

          (:keyup (:keysym keysym)
           (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
             (sdl2:push-event :quit)))

          (:keydown (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (cond ((sdl2:scancode= scancode :scancode-up)
                    (setf (ship-vel *ship*) (vec-minus-y (ship-vel *ship*) 
                                                         (ship-thrust *ship*))))

                   ((sdl2:scancode= scancode :scancode-down)
                    (setf (ship-vel *ship*) (vec-add-y (ship-vel *ship*)
                                                       (ship-thrust *ship*))))

                   ((sdl2:scancode= scancode :scancode-left)
                    (setf (ship-vel *ship*) (vec-minus-x (ship-vel *ship*)
                                                         (ship-thrust *ship*))))

                   ((sdl2:scancode= scancode :scancode-right)
                    (setf (ship-vel *ship*) (vec-add-x (ship-vel *ship*)
                                                       (ship-thrust *ship*))))

                   ((sdl2:scancode= scancode :scancode-a)
                    (setf (ship-heading *ship*) (rotate-ship-ccw *ship*
                                                                 (/ pi 60))))

                   ((sdl2:scancode= scancode :scancode-d)
                    (setf (ship-heading *ship*)  (rotate-ship-cw *ship*
                                                                 (/ pi 60))))

                   ((sdl2:scancode= scancode :scancode-r)
                    (setf (ship-pos *ship*) #(0 0)
                          (ship-vel *ship*) #(0 0))))))

          (:idle ()
           (update)
           (render renderer)
           (sdl2:delay 16))

          (:quit ()
           (when (plusp (sdl2-ttf:was-init))
             (sdl2-ttf:quit))
           t))))))
