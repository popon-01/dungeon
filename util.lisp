(in-package dungeon)

;;;utility
(defmacro whens (&body body)
  `(progn
     ,@(loop for b in body collect `(when ,@b))))


(defmacro pmif (test num)
  (with-gensyms (gnum)
    `(let ((,gnum ,num))
       (if ,test ,gnum (- ,gnum)))))

(defmacro alambda (vars &body body)
  `(labels ((self ,vars ,@body))
     #'self))

(defmacro letrec (vars &body body)
  `(labels ((rec ,(mapcar #'first vars) ,@body))
     (rec ,@(mapcar #'second vars))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;to-string
(defun to-s (obj)
  (format nil "~a" obj))

;print if test is true
(defun print-if (test exp)
  (if test (print exp) exp))

;SOUATARI
(defun round-robin (fn lis)
  (mapl (lambda (xs) (mapcar (lambda (x) (funcall fn (car xs) x))
			     (cdr xs)))
	lis))

(defmacro mapfn (&rest expr)
  (with-gensyms (gs)
    `(lambda (&rest ,gs)
       (apply #',(car expr)
	      (mapcar (fn ,(cdr expr)) ,gs)))))

(defmacro fn (expr) `#',(rbuild expr))
(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
	  (build-compose (cdr expr))
	  (build-call (car expr) (cdr expr)))))
(defun build-call (op fns)
  (with-gensyms (g)
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
			  `(,(rbuild f) ,g))
		      fns)))))
(defun build-compose (fns)
  (with-gensyms (g)
    `(lambda (,g)
       ,(labels ((rec (fns)
		      (if fns
			  `(,(rbuild (car fns))
			     ,(rec (cdr fns)))
			  g)))
		(rec fns)))))

;;;clos;;;

;define class
(defmacro define-class (class-name parent  &rest res)
  `(defclass ,class-name ,parent
     ,(mapcar (lambda (lis)
		(if (listp lis)
		    (apply(lambda (x &optional (y nil) (z x))
			    `(,x :initarg 
				 ,(intern (symbol-name x) "KEYWORD") 
				 :initform ,y :accessor ,z))
			  lis)
		    ((lambda (x) 
		       `(,x :initarg 
			    ,(intern (symbol-name x) "KEYWORD") 
			    :initform nil :accessor ,x))
		     lis)))
	      res)))

;get slot list
(defun slot-list (instance)
  (mapcar #'c2mop:slot-definition-name
	  (c2mop:class-slots (class-of instance))))

;apply func to all slot
(defun nmapslot (fn instance)
  (dolist (slot (slot-list instance))
    (setf (slot-value instance slot)
	  (funcall fn (slot-value instance slot)))))

;define method between class a and class b
(defmacro definteract-method (method-name arg1 arg2 &body body)
  `(progn
     (defmethod ,method-name (,arg1 ,arg2)
       ,@body)
     (defmethod ,method-name (,arg2 ,arg1)
       (,method-name ,(car arg1) ,(car arg2)))))

;;;math;;;

;convert radian to degree
(defun rad->degree (degree) (* pi (/ degree 180.0)))

;norm of vector(x,y)
(defun vec-abs (x y)
  (sqrt (+ (* x x) (* y y))))

;euclid distance of (x1,y1) to (x2,y2)
(defun euc-dist (x1 y1 x2 y2)
  (vec-abs (- x1 x2) (- y1 y2)))

;unit vector of vector(x,y)
(defun univec (x y)
  (let ((dist (vec-abs x y)))
    (if (zerop dist)
	(list 0.0 0.0)
	(list (/ x dist) (/ y dist)))))

;unit vector of (sx sy) to (tx ty)
(defun dir-univec (sx sy tx ty)
  (let ((vec (univec (- tx sx) (- ty sy))))
    (list (first vec) (second vec))))

;;;timer;;;

;count num loop 
(defun make-timer (num)
  (let ((i 0))
    (lambda ()
      (if (<= num i)
	  (progn (setf i 1) t)
	  (progn (incf i) nil)))))

;count num loop if not charged
(defun charge-timer (num)
  (let ((charge num))
    (lambda (message)
      (case message
	(:charge (progn (setf charge (clamp (1+ charge) 0 num))
			(<= num charge)))
	(:shot (and (<= num charge)
		    (setf charge 0)
		    t))))))

;;;others;;;

;vector of instance a to instance b
(defun a-to-b-vector (a b &optional (xfun #'get-x) (yfun #'get-y))
  (list (- (funcall xfun b) (funcall xfun a))
	(- (funcall yfun b) (funcall yfun a))))

;distance of instance a to instance b
(defun distance (a b &optional (xfun #'get-x) (yfun #'get-y))
  (sqrt (+ (expt (- (funcall xfun a) (funcall xfun b)) 2)
	   (expt (- (funcall yfun a) (funcall yfun b)) 2))))

;unit vector of instance a to instance b
(defun uvec (a b &optional (xfun #'get-x) (yfun #'get-y))
  (let ((dis (distance a b xfun yfun)))
    (mapcar (lambda (x) (float (/ x dis)))
	    (a-to-b-vector a b xfun yfun))))


(defmacro defcollide (arg1 arg2 &body body)
  (if (some (lambda (x) (eq (car body) x)) 
	    '(:before :after :around))
      `(progn
	 (defmethod collide ,(car body) (,arg1 ,arg2 game)
	   ,@(cdr body))
	 (defmethod collide ,(car body) (,arg2 ,arg1 game)
	   ,@(cdr body)))
      `(progn
	 (defmethod collide (,arg1 ,arg2 game)
	   ,@body)
	 (defmethod collide (,arg2 ,arg1 game)
	   ,@body))))

(defmacro defstate (name lambda-list &body body)
  (with-gensyms (glis)
    `(defun ,name (&rest ,glis)
       (if (and (not (cdr ,glis)) (eq :symbol (car ,glis)))
	   ',name
	   (destructuring-bind ,lambda-list ,glis
	     ,@body)))))

;turn image by vx
(defun image-turn (obj)
  (setf (image obj) 
	(if (plusp (vx obj)) 
	    (image-r obj)
	    (image-l obj))))
