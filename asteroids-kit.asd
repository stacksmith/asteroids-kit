;;;; asteroids-kit.asd

(asdf:defsystem #:asteroids-kit
  :description "Describe asteroids-kit here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:lispbuilder-sdl
               #:lispbuilder-sdl-gfx)
  :serial t
  :components ((:file "package")
               (:file "asteroids-kit")
	       (:file "geo")))

