(defun vec-x (vector)
  (aref vector 0))

(defun vec-y (vector)
  (aref vector 1))

(defun polar-to-cart (r theta)
  (vector (* r (cos theta)) (* r (sin theta))))

(defun vec-add (u v)
  (assert (= (length u) (length v)))
  (map 'vector (lambda (u v) (+ u v)) u v))

(defun vec-subtract (u v)
  (assert (= (length u) (length v)))
  (map 'vector (lambda (u v) (- u v)) u v))

(defun vec-add-idx (u k i)
  "Add K to index I of vector U"
  (assert (> (length u) i))
  (let ((vec (copy-seq u)))
    (setf (aref vec i) (+ (aref vec i) k))
    vec))

(defun vec-subtract-idx (u k i)
  "Subtract K from index I of vector U"
  (vec-add-idx u (- k) i))

(defun vec-add-x (u k)
  "Add K to the x component of U"
  (vec-add-idx u k 0))

(defun vec-subtract-x (u k)
  (vec-subtract-idx u k 0))

(defun vec-add-y (u k)
  "Add K to the y component of U"
  (vec-add-idx u k 1))

(defun vec-subtract-y (u k)
  (vec-subtract-idx u k 1))

(defun vec-scale (u k)
  "Scale vector U by scale factor K"
  (map 'vector (lambda (e) (* e k)) u))

(defun l1 (u v)
  "L1 norm between U and V"
  (reduce #'+ (map 'vector (lambda (e) (abs e)) (vec-subtract u v))))

(defun l2 (u v)
  "L2 norm between U and V"
  (sqrt (reduce #'+ (map 'vector (lambda (e) (* e e)) (vec-subtract u v)))))

(defun vec-dot (u v)
  "Calculate dot product between vectors U and V"
  (reduce #'+ (map 'vector (lambda (u v) (* u v)) u v)))

(defun vec-norm (u)
  "Return the norm, or magnitude, of vector U"
  (sqrt (vec-dot u u)))

(defun normalized (u)
  "Return normalized vector U"
  (vec-scale u (/ (vec-norm u))))

