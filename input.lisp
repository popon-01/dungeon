(in-package :dungeon)

(defgeneric update-key-state (key keypress keystate))

(defun key-pressed-p (key)
  (oddp key))
(defun key-down-p (key)
  (= key 3))
(defun key-up-p (key)
  (= key 2))

(defmacro defkeystate (name &rest keymaps)
  (with-gensyms (key key-press key-state)
    `(progn
       (defclass ,name ()
	  ,(mapcar (lambda (x) `(,(car x) :initform 0)) keymaps))
       (defmethod update-key-state (,key ,key-press (,key-state ,name))
	  (with-slots ,(mapcar #'car keymaps) ,key-state
	    (cond ,@(mapcar (lambda (keys)
			     `((sdl:key= ,key ,(cadr keys))
			       (setf ,(car keys) ,key-press)))
			   keymaps))))
       (defmethod next-key-state ((,key-state ,name))
	 (nmapslot (lambda (x) (mod x 2)) ,key-state)))))

(defkeystate game-key
    (enter :sdl-key-return)
    (right :sdl-key-right)
    (left :sdl-key-left)
    (up :sdl-key-up)
    (down :sdl-key-down))
