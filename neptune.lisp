(load "vector.lisp")
(load "physics.lisp")

(load "planet.lisp")
(load "ship.lisp")

(load "render.lisp")

(in-package :neptune)

(defparameter *screen-width* 800)
(defparameter *screen-height* 600)

(defparameter *camera* #(300 400))

(defparameter *player* (make-ship :name "cherokee mist"
                                :pos #(200 100)
                                :mass 1
                                :thrust #(0 0)
                                :engine-thrust 1000
                                :size 5))

(defparameter *earth* (make-planet :name "earth"
                                   :pos #(200 300)
                                   :mass 100000
                                   :radius 200))

(defparameter *moon* (make-planet :name "moon"
                                  :pos #(200 100)
                                  :vel #(20 0)
                                  :mass 100
                                  :radius 10))

(defparameter *europa* (make-planet :name "europa"
                                    :pos #(400 300)
                                    :vel #(0 20)
                                    :mass 100
                                    :radius 20))

(defparameter *planets* (list *earth* *moon* *europa*))

(defun world-to-screen (position)
  (let ((coords (vec:subtract position *camera*)))
    (vec:add coords (vector (floor *screen-width* 2)
                            (floor *screen-height* 2)))))

(defun update (delta-time-us)
  "Update objects"
  
  (update-ship *player* *earth* delta-time-us)

  (setf *camera* (ship-pos *player*))

  (loop for planet in *planets* do
        (update-planet planet *planets* delta-time-us)))

; TODO: macroise render functions

(defun render-planet (renderer planet)
  (let ((coords (world-to-screen (planet-pos planet))))
    (draw-circle renderer
                 (floor (vec:vec-x coords))
                 (floor (vec:vec-y coords))
                 (planet-radius planet))))

(defun render-direction-vector (renderer ship)
  (let ((coords (world-to-screen (ship-pos ship))))
    (let ((dir (polar-to-cart 1 (ship-heading ship))))
      (sdl2:render-draw-line
        renderer
        (floor (vec:vec-x coords))
        (floor (vec:vec-y coords))
        (floor (+ (vec:vec-x coords) (* 10 (vec:vec-x dir))))
        (floor (+ (vec:vec-y coords) (* 10 (vec:vec-y dir))))))))

(defun render-ship-velocity-vector (renderer ship)
  (render-point-vector renderer
                       (world-to-screen (ship-pos ship))
                       (ship-vel ship)))

(defun render-ship (renderer ship)
  (let ((coords (world-to-screen (ship-pos ship))))
    (draw-circle renderer
                 (floor (vec:vec-x coords))
                 (floor (vec:vec-y coords))
                 (ship-size ship))
    (render-direction-vector renderer ship)
    (render-ship-velocity-vector renderer ship)))

(defun render-direction-to (renderer ship planet)
  (let ((ship-pos (world-to-screen (ship-pos ship)))
        (direction-vector (vec:subtract (planet-pos planet) (ship-pos ship)))
        endpoint)
    (setf endpoint (world-to-screen
                     (vec:add (ship-pos ship)
                              (vec:scale (vec:normalized direction-vector) 20))))
    
    (sdl2:render-draw-line
      renderer
      (floor (vec:vec-x ship-pos))
      (floor (vec:vec-y ship-pos))
      (floor (vec:vec-x endpoint))
      (floor (vec:vec-y endpoint)))))

(defun render (renderer)
  (sdl2:set-render-draw-color renderer 0 0 0 255)
  (sdl2:render-clear renderer)

  (sdl2:set-render-draw-color renderer 0 255 0 255)

  (mapcar (lambda (planet) (render-planet renderer planet)) *planets*)

  (render-direction-to renderer *player* *earth*)
  (render-ship renderer *player*)

  (sdl2:render-present renderer))

(defun keyup-handler (scancode dt)
  (declare (ignore dt))
  (cond ((sdl2:scancode= scancode :scancode-escape)
         (sdl2:push-event :quit))
        
        ((sdl2:scancode= scancode :scancode-up)
         (setf (ship-thrust *player*) #(0 0)))

        ((sdl2:scancode= scancode :scancode-down)
         (setf (ship-thrust *player*) #(0 0)))))

(defun keydown-handler (scancode dt)
  (cond ((sdl2:scancode= scancode :scancode-up)
         (setf (ship-thrust *player*)
               (vec:scale (polar-to-cart 1 (ship-heading *player*))
                          (* (ship-engine-thrust *player*)
                             (/ dt 1000000)))))
        
        ((sdl2:scancode= scancode :scancode-down)
         (setf (ship-thrust *player*)
               (vec:scale (polar-to-cart 1 (- (ship-heading *player*) pi))
                          (* (ship-engine-thrust *player*)
                             (/ dt 1000000)))))

        ((sdl2:scancode= scancode :scancode-left)
         (setf (ship-vel *player*)
               (vec:subtract-x (ship-vel *player*)
                          (* (/ dt 1000000) (ship-thrust *player*)))))

        ((sdl2:scancode= scancode :scancode-right)
         (setf (ship-vel *player*)
               (vec:add-x (ship-vel *player*)
                          (* (/ dt 1000000) (ship-thrust *player*)))))

        ((sdl2:scancode= scancode :scancode-a)
         (setf (ship-heading *player*) (rotate-ship-ccw *player*
                                                        (/ pi 60))))

        ((sdl2:scancode= scancode :scancode-d)
         (setf (ship-heading *player*)  (rotate-ship-cw *player*
                                                        (/ pi 60))))

        ((sdl2:scancode= scancode :scancode-r)
         (setf (ship-pos *player*) #(0 0)
               (ship-vel *player*) #(0 0)))))

(defun main ()
  (sdl2:with-init (:everything)
    (sdl2-ttf:init)
    (sdl2:with-window (window :title "valleys of neptune"
                              :w *screen-width*
                              :h *screen-height*)

      (sdl2:with-renderer (renderer window)

        (let ((previous-tick-us (get-internal-real-time))
              current-tick-us
              delta-time-us)

          (sdl2:with-event-loop (:method :poll)

            (:idle ()
             (format t "Ship heading: ~D~%" (ship-heading *player*))

             (setf current-tick-us (get-internal-real-time)
                   delta-time-us (- current-tick-us previous-tick-us)
                   previous-tick-us current-tick-us)
             
             (update delta-time-us)
             (render renderer)
             (sdl2:delay 16))

            (:keyup (:keysym keysym)
             (keyup-handler (sdl2:scancode-value keysym) delta-time-us))

            (:keydown (:keysym keysym)
             (keydown-handler (sdl2:scancode-value keysym) delta-time-us))

            (:quit ()
             (when (plusp (sdl2-ttf:was-init))
               (sdl2-ttf:quit))
             t)))))))
