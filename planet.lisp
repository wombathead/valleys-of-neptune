(in-package :neptune)

(defstruct planet
  name
  pos
  (vel #(0 0))
  radius
  mass)

(defun planet-pos-x (planet)
  (vec:vec-x (planet-pos planet)))

(defun planet-pos-y (planet)
  (vec:vec-y (planet-pos planet)))

(defun planet-attractive-force (p q)
  "Compute the attractive force Q exerts on P"
  (phx:attractive-force (planet-pos p) (planet-mass p)
                        (planet-pos q) (planet-mass q)))

(defun resultant-gravitational-force (planet neighbourhood)
  "Calculate the net force exerted on PLANET by surrounding NEIGHBOURHOOD"
  (let ((resultant-force #(0 0)))
    (loop for neighbour in neighbourhood 
          unless (eql neighbour planet) do
          (setf resultant-force (vec:add resultant-force
                                         (planet-attractive-force planet neighbour))))
    resultant-force))

(defun updated-planet-velocity (planet neighbourhood dt)
  (let ((gravitational-force (resultant-gravitational-force planet neighbourhood))
        acceleration)
    (setf acceleration (phx:acceleration gravitational-force (planet-mass planet)))
    (vec:add (planet-vel planet) (vec:scale acceleration
                                            (/ dt 1000000)))))

(defun updated-planet-position (planet dt)
  (vec:add (planet-pos planet) (vec:scale (planet-vel planet)
                                          (/ dt 1000000))))

(defun update-planet (planet neighbourhood dt)
  (setf (planet-vel planet) (updated-planet-velocity planet neighbourhood dt))
  (setf (planet-pos planet) (updated-planet-position planet dt)))
