;;;; asteroids-kit.asd

(asdf:defsystem #:asteroids-kit
  :description "A repl environment for vector game hacking"
  :author "stack@apple2.x10.mx"
  :license "WTFPL"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx)
  :serial t
  :components ((:file "package")
               (:file "asteroids-kit")
	       (:file "swankstuff")
	       (:file "geo")))

