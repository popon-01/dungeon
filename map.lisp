(in-package dungeon)

(defun load-map (file-name game)
  (cond ((string= "txt" (pathname-type file-name))
	 (load-txt-map file-name game))
	((string= "stg" (pathname-type file-name))
	 (load-stg-map file-name game))
	(t (error "~a is not stage file." file-name))))

(defun load-txt-map (file-name game)
  (setf (map-size game) (list 0 0))
  (iter (for h upfrom 16 by 32)
	(for line in-file file-name using #'read-line)
	(setf (second (map-size game)) (+ 16 h))
	(appending
	 (iter (for w upfrom 16 by 32)
	       (for code in (split-sequence #\space line))
	       (setf (first (map-size game)) (+ 16 w))
	       (push-game-object (make-game-object code w h)
				 game)))))

(defun load-stg-map (file-name game)
  (setf (map-size game) (list 0 0)) 
  (iter (for h upfrom 16 by 32)
	(for line in-file file-name using #'read-line)
	(setf (second (map-size game)) (+ 16 h)
	      (first (map-size game)) (length line))
	(appending
	 (iter (for w upfrom 16 by 32)
	       (for code in-string line)
	       (setf (first (map-size game)) (+ 16 w))
	       (push-game-object (make-game-object 
				  (string code) w h)
				 game)))))

(defmacro map-char-table (&rest table)
  `(cond ,@(mapcar (lambda (xs)
		     `((string= ,(car xs) code)
		       ,(let* ((delta (nth 3 xs))
			       (dx (if delta (first delta) 0))
			       (dy (if delta (second delta) 0)))
			      `(make-instance ',(second xs)
					      ,@(nth 2 xs) 
					      :x (+ x ,dx) 
					      :y (+ y ,dy))))) table)))

(defun make-game-object (code x y)
  (map-char-table
   ("p" player)))


(defmethod push-game-object ((obj null) game))
(defmethod push-game-object ((obj gameobject) (game game))
  (push obj (objects game)))
