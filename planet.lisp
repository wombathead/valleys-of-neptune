(load "linalg.lisp")

(defparameter +gravitational-constant+ 0.01)

(defstruct planet
  name
  pos
  (vel #(0 0))
  radius
  mass)

(defun planet-pos-x (planet)
  (vec-x (planet-pos planet)))

(defun planet-pos-y (planet)
  (vec-y (planet-pos planet)))

(defun attractive-force (p q)
  "Compute the attractive force Q exerts on P"
  (let ((r (l2 (planet-pos p) (planet-pos q)))
        (resultant (normalized (vec-subtract (planet-pos q) (planet-pos p))))
        magnitude)
    (setf magnitude (/ (* +gravitational-constant+ (planet-mass p) (planet-mass q))
                       (* r r)))
    (vec-scale resultant magnitude)))

(defun acceleration (force mass)
  "Calculate the accerlation that FORCE induces on PLANET"
  (vec-scale force (/ mass)))

(defun update-planet-position (planet planets)
  "Calculate the net force exerted on PLANET by surrounding PLANETS"
  (let (resultant)
    (loop for p in planets 
          unless (eql p planet) do
          (setf resultant (vec-add (planet-vel planet)
                                   (acceleration (attractive-force planet p)
                                                 (planet-mass planet)))))
    (setf (planet-vel planet) resultant)
    (setf (planet-pos planet) (vec-add (planet-pos planet) (planet-vel planet)))))
