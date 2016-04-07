;;;; dungeon.asd

(asdf:defsystem :dungeon
  :description "Describe dungeon here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "global")
	       (:file "image")
	       (:file "input")
	       (:file "state")
	       (:file "game")
	       (:file "gameobject")
	       (:file "player")
	       (:file "camera")
               (:file "main"))
  :depends-on (:lispbuilder-sdl :split-sequence :iterate :alexandria :closer-mop))

