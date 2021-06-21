(load "linalg.lisp")

(defstruct player
  name
  (pos (vector (floor *screen-width* 2) (floor *screen-height* 2)))
  (vel #(0 0))
  thrust
  size)

(defun player-pos-x (player)
  (vec-x (player-pos player)))

(defun player-pos-y (player)
  (vec-y (player-pos player)))

(defun player-vel-x (player)
  (vec-x (player-vel player)))
   
(defun player-vel-y (player)
  (vec-y (player-vel player)))

