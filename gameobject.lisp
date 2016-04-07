(in-package :dungeon)

(defgeneric updete-object (object game))
(defgeneric draw-object (object game))

(defun update-all (game)
  (dolist (obj (objects game))
    (update-object obj game))
  (setf (objects game) (remove-if-not #'alive (objects game))))

(defun draw-all (game)
  (dolist (obj (objects game))
    (draw-object obj game)))

(define-class gameobject ()
  width height 
  (x 0 get-x)
  (y 0 get-y)
  (vx 0) (vy 0) 
  (alive t) 
  image 
  (ani-time 0)
  cell-num
  (ani-frame 1))

(defmethod initialize-instance :after ((obj gameobject) &key)
  (with-slots (image width height cell-num) obj
    (setf cell-num (length (sdl:cells image)))
    (if (> cell-num 1)
	(setf width (sdl:width (elt (sdl:cells image) 0))
	      height (sdl:height (elt (sdl:cells image) 0)))
	(setf width (sdl:width image)
	      height (sdl:height image)))))

(defmethod draw-object ((obj gameobject) game)
  (with-slots (x y width height image
		 ani-time cell-num ani-frame) obj
    (let ((nowcell (mod (truncate ani-time ani-frame) cell-num)))
      (when (or (not (typep obj 'gamecharacter)) 
		(not (muteki obj))
		(not (zerop (mod ani-time 3))))
	  (sdl:draw-surface-at-* image
				 (- (round (x-in-camera x game)) 
				    (truncate width 2))
				 (- (round (y-in-camera y game)) 
				    (truncate height 2))
				 :cell nowcell))
      (setf (ani-time obj) 
	    (mod (1+ ani-time) (* cell-num ani-frame 3))))))

(defmethod update-object ((obj gameobject) game)
  (incf (get-x obj) (vx obj))
  (incf (get-y obj) (vy obj))
  (when (out-of-map-p obj game)
    (kill obj game)))

(defmethod kill ((obj gameobject) game) (setf (alive obj) nil))

(defmethod out-of-map-p ((obj gameobject) game)
  (let ((border 40))
    (not (and (< (- border) (get-x obj) 
		 (+ (first (map-size game)) border))
	      (< (- border) (get-y obj)
		 (+ (second (map-size game)) border))))))
