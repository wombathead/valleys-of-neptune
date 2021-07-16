(in-package :neptune)

; TODO: move to another file
(defparameter +gravity+ 0.01)

(defstruct ship
  name
  pos
  (vel #(0 0))
  (heading 0)   ; heading in radians
  engine-thrust ; scalar representing engine power
  thrust        ; vector representing force from engine
  mass
  size)

(defun ship-pos-x (ship)
  (vec:vec-x (ship-pos ship)))

(defun ship-pos-y (ship)
  (vec:vec-y (ship-pos ship)))

(defun ship-vel-x (ship)
  (vec:vec-x (ship-vel ship)))

(defun ship-vel-y (ship)
  (vec:vec-y (ship-vel ship)))

(defun apply-acceleration (ship acceleration dt)
  (vec:add (ship-vel ship) (vec:scale acceleration (/ dt 1000000))))

(defun gravitational-force (ship planet)
  "Calculate the force that PLANET exerts on SHIP"
  (phx:attractive-force (ship-pos ship) (ship-mass ship)
                        (planet-pos planet) (planet-mass planet)))

(defun updated-ship-velocity (ship planet dt)
  (let ((gravitational-force (gravitational-force ship planet))
        resultant-force
        acceleration)
    (setf resultant-force (vec:add
                            gravitational-force
                            (ship-thrust ship)))
    (setf acceleration (phx:acceleration resultant-force (ship-mass ship)))
    (vec:add (ship-vel ship) (vec:scale acceleration (/ dt 1000000)))))

(defun updated-ship-position (ship dt)
  "Update ship position based on position and velocity"
  (vec:add (ship-pos ship) (vec:scale (ship-vel ship) (/ dt 1000000))))

(defun update-ship (ship nearest-planet dt)
  (setf (ship-vel ship) (updated-ship-velocity ship nearest-planet dt))
  (setf (ship-pos ship) (updated-ship-position ship dt)))

(defun rotate-ship-ccw (ship theta)
  "Rotate ship's heading THETA degrees counter-clockwise"
  (- (ship-heading ship) theta))

(defun rotate-ship-cw (ship theta)
  (rotate-ship-ccw ship (- theta)))
