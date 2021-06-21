(load "linalg.lisp")

; TODO: move to another file
(defparameter +gravity+ 0.01)

(defstruct ship
  name
  pos
  (vel #(0 0))
  (heading 0)
  thrust
  size)

(defun ship-pos-x (ship)
  (vec-x (ship-pos ship)))

(defun ship-pos-y (ship)
  (vec-y (ship-pos ship)))

(defun ship-vel-x (ship)
  (vec-x (ship-vel ship)))

(defun ship-vel-y (ship)
  (vec-y (ship-vel ship)))

(defun update-ship-velocity (ship)
  (vec-add-y (ship-vel ship) +gravity+))

(defun update-ship-position (ship)
  "Update ship position based on position and velocity"
  (vec-add (ship-pos ship) (ship-vel ship)))

(defun update-ship (ship)
  ; (setf (ship-vel ship) (update-ship-velocity ship))
  (setf (ship-pos ship) (update-ship-position ship)))

(defun rotate-ship-ccw (ship theta)
  "Rotate ship's heading THETA degrees counter-clockwise"
  (- (ship-heading ship) theta))

(defun rotate-ship-cw (ship theta)
  (rotate-ship-ccw ship (- theta)))
