(in-package :dungeon)

(define-class game ()
  (window-size '(640 480))
  (map-size '(1280 960))
  (player nil)
  (camera '(0 0))
  (objects nil)
  (state :game)
  (state-func nil)
  (keystate (make-instance 'game-key)))

(defmethod initialize-instance :after ((game game) &key)
  (update-state game))

(defun run-state (game)
  (funcall (state-func game) game))

(defun update-state (game)
  (setf (state-func game)
	(case (state game)
	  (:title #'title-state)
	  (:game #'game-state))))

(defun change-state (sym game)
  (setf (state game) sym)
  (update-state game))
