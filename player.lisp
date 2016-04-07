(in-package :dungeon)

(define-class player (gameobject)
  (image (get-image :player))
  (velocity 5)
  (vx 0) (vy 0))


(defmacro calc-velocity (velocity flag-keys)
  `(if (or ,@(loop for k in flag-keys 
		collect `(key-pressed-p ,k)))
       (* ,velocity (sqrt 2) 0.5)
       ,velocity))

(defun player-keyevents (player game)
  (with-slots (vx vy velocity) player
    (with-slots (right left up down) (keystate game)
      (whens
	((key-pressed-p right) 
	 (incf vx (calc-velocity velocity (up down))))
	((key-pressed-p left)
	 (incf vx (calc-velocity (- velocity) (up down))))
	((key-pressed-p down)
	 (incf vy (calc-velocity velocity (right left))))
	((key-pressed-p up) 
	 (incf vy (calc-velocity (- velocity) (right left))))))))



(defmethod update-object ((player player) game)
  (call-next-method)
  (setf (vx player) 0
	(vy player) 0)
  (player-keyevents player game)) 

