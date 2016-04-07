(in-package :dungeon)

(defvar *current-path* (load-time-value
			(or #.*compile-file-pathname* 
			    *load-pathname*)))
(defvar *dir-path* (directory-namestring *current-path*))
(defvar *lib-path* (merge-pathnames "lib/" *dir-path*))
