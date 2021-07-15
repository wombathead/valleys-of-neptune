(ql:quickload :sdl2)
(ql:quickload :sdl2-ttf)

(defpackage :vec
  (:use :cl)
  (:export :vec-x
           :vec-y
           :zero-vector-p
           :add
           :subtract
           :add-x
           :add-y
           :subtract-x
           :subtract-y
           :scale
           :l1
           :l2
           :dot
           :norm
           :normalized
           :polar-to-cart))

(defpackage :phx
  (:use :cl :vec)
  (:export :attractive-force
           :acceleration))

(defpackage :valleys-of-neptune
  (:nicknames :neptune)
  (:use :cl :cl-user :sdl2 :vec :phx)
  (:export :main))
