(in-package :phx)

(defconstant +gravitational-constant+ 1)

(defun attractive-force (pos-a mass-a pos-b mass-b)
  "Compute attractive force that B exerts on A"
  ;; TODO: constant as a parameter (e.g. to allow different attractive forces
  ;; of differing strengths?
  (let ((r (l2 pos-a pos-b))
        (direction (normalized (vec:subtract pos-b pos-a)))
        magnitude)
    (setf magnitude (/ (* +gravitational-constant+ mass-a mass-b)
                       (* r r)))
    (vec:scale direction magnitude)))

(defun acceleration (force mass)
  "Acceleration that FORCE induces on MASS"
  (vec:scale force (/ mass)))
