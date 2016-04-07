(in-package :dungeon)

;;;;;main;;;;;
(defun run()
  (sdl:with-init ()
    (sdl:window 640 480 :title-caption "test")
    (setf (sdl:frame-rate) 60)
    (load-game-image)
    (let ((game (make-instance 'game)))
      (init-camera game)
      (sdl:update-display)
      (sdl:with-events ()
	(:quit-event () t)
	(:key-down-event (:key key)
			 (when (sdl:key= key :sdl-key-escape)
			     (sdl:push-quit-event))
			 (update-key-state key 3
			    (keystate game)))
	(:key-up-event (:key key)
		       (update-key-state key 2
			   (keystate game)))
	(:idle ()
	       (run-state game)
	       (sdl:update-display)
	       (next-key-state (keystate game)))))))

