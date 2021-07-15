(ql:quickload :asdf)

(asdf:defsystem "neptune"
  :depends-on ("sdl2")
  :components ((:file "package")
               (:file "neptune")
               (:file "vector")
               (:file "physics")))
