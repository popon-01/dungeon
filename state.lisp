(in-package :dungeon)

;reset game status
(defun start-game ())

(defun title-state (game)
  (with-slots (enter) (keystate game)
    (when (key-pressed-p enter)
      (change-state :game game)
      (let ((player (make-instance 'player)))
	(setf (player game) player)
	(push player (objects game))))))

(defun game-state (game)
  (update-all game)
  (update-camera game)
  (sdl:clear-display sdl:*black*)
  (draw-all game))
