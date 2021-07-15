(in-package :vec)

(defun vec-x (vector)
  (aref vector 0))

(defun vec-y (vector)
  (aref vector 1))

(defun zero-vector-p (u)
  (loop for i across u do
        (unless (zerop i)
          (return-from zero-vector-p nil)))
  u)

(defun polar-to-cart (r theta)
  (vector (* r (cos theta)) (* r (sin theta))))

(defun add (u v)
  "Add two vectors U and V"
  (assert (= (length u) (length v)))
  (map 'vector (lambda (u v) (+ u v)) u v))

(defun subtract (u v)
  "Subtract two vectors U and V"
  (assert (= (length u) (length v)))
  (map 'vector (lambda (u v) (- u v)) u v))

(defun add-idx (u k i)
  "Add K to index I of vector U"
  (assert (> (length u) i))
  (let ((vec (copy-seq u)))
    (setf (aref vec i) (+ (aref vec i) k))
    vec))

(defun subtract-idx (u k i)
  "Subtract K from index I of vector U"
  (add-idx u (- k) i))

(defun add-x (u k)
  "Add K to the x component of U"
  (add-idx u k 0))

(defun subtract-x (u k)
  "Subtract K from the x component of U"
  (subtract-idx u k 0))

(defun add-y (u k)
  "Add K to the y component of U"
  (add-idx u k 1))

(defun subtract-y (u k)
  "Subtract K from the y component of U"
  (subtract-idx u k 1))

(defun scale (u k)
  "Scale vector U by scale factor K"
  (map 'vector (lambda (e) (* e k)) u))

(defun l1 (u v)
  "L1 norm between U and V"
  (reduce #'+ (map 'vector (lambda (e) (abs e)) (subtract u v))))

(defun l2 (u v)
  "L2 norm between U and V"
  (sqrt (reduce #'+ (map 'vector (lambda (e) (* e e)) (subtract u v)))))

(defun dot (u v)
  "Calculate dot product between vectors U and V"
  (reduce #'+ (map 'vector (lambda (u v) (* u v)) u v)))

(defun norm (u)
  "Return the norm, or magnitude, of vector U"
  (sqrt (dot u u)))

(defun normalized (u)
  "Return normalized vector U"
  (if (zero-vector-p u)
      u
      (scale u (/ (norm u)))))
