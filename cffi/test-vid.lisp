(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn 
    (setf asdf:*central-registry*
	  (union (list *default-pathname-defaults*)
		 asdf:*central-registry*))
    (asdf:operate 'asdf:load-op 'v4l2)
    (asdf:operate 'asdf:load-op 'arduino-serial)))

(defpackage :g
  (:use :cl :v4l2))

(in-package :g)

#+nil
(defparameter *ard* (serial:arduino-init))

#+nil
(destructuring-bind (str fd) *ard*
  (let ((s
	 (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				:external-format :latin-1 
				:buffering :full)))
    (serial:talk-arduino fd s (string-downcase
			(format nil "~a" `(dac 2048 ,(+ 0 2048)))))))

(defvar *ard* nil)
(defun set-dac (a b)
  "a, b are 12-bit values that will be written to the DAC. 2048 is the center."
  (when *ard*
   (destructuring-bind (str fd) *ard*
     (let ((s
	    (sb-sys:make-fd-stream fd :input t :output t :element-type 'base-char
				   :external-format :latin-1 
				   :buffering :full)))
       (serial:talk-arduino fd s (string-downcase
				  (format nil "~a" `(dac ,a ,b))))))))


#+nil
(set-dac 2048 2048)


#+nil
(v4l-init :w 800 :h 600)
#+nil
(v4l-uninit)

#+nil
(defparameter *blub*
  (loop for j from -100 upto 470 by 10 do
   (loop for i from -210 upto 310 by 10 do
	(set-dac (+ 2048 i) (+ 2048 j))
	(let ((r (wait-and-read-frame))
	      (w 800)
	      (h 600)
	      (c 2))
	  (destructuring-bind (ap n length) *buffers*
	    (assert (< r n))
	    (assert (= length (* w h c)))
	    (let* ((a (make-array (list h w c) :element-type '(unsigned-byte 8)))
		   (a2 (make-array (list h (* c w)) :element-type '(unsigned-byte 8)
				   :displaced-to a))
		   (a1 (make-array length :element-type '(unsigned-byte 8)
				   :displaced-to a)))
	      (dotimes (i length)
		(setf (aref a1 i) (cffi:mem-aref ap :uchar (+ (* r length) i))))
	      (write-pgm (format nil "/dev/shm/o~3,'0d-~3,'0d.pgm" (+ 2048 i) (+ 2048 j)) a2)
	      (progn 
		(let* ((y (make-array (list h w) :element-type '(unsigned-byte 8))))
		  (dotimes (j h)
		    (dotimes (i w)
		      (setf (aref y j i) (aref a j i 0))))
		  (let* ((w (/ w 2))
			 (v (make-array (list h w) :element-type '(unsigned-byte 8)))
			 (r (make-array (list h w) :element-type '(unsigned-byte 8))))
		    (dotimes (j h)
		      (dotimes (i w)
			(setf (aref v j i) (aref a j (+ 1 (* 2 i)) 1))))
		  
		    (dotimes (j h)
		      (dotimes (i w)
			(setf (aref r j i) (min 255 (max 0 (floor (+ (aref y j (* 2 i)) 
								     (* 1.5748 (+ -128 (aref v j i))))))))))
		    (write-pgm (format nil "/dev/shm/r~3,'0d-~3,'0d.pgm" (+ 2048 i) (+ 2048 j)) r))))
	      (format t "~a~%" (list i j))
	      nil))))))

;; acquisition time 22:12 to 22:29 


(declaim (optimize (speed 0) (safety 3) (debug 3)))

#+nil
(defparameter *q*
  (let ((r (first *blub*))
	(w 1600)
	(h 1200)
	(c 2))
    (destructuring-bind (ap n length) *buffers*
      (assert (< r n))
      (assert (= length (* w h c)))
      (let* ((a (make-array (list h w c) :element-type '(unsigned-byte 8)))
	     (a1 (make-array length :element-type '(unsigned-byte 8)
			     :displaced-to a)))
	(dotimes (i length)
	  (setf (aref a1 i) (cffi:mem-aref ap :uchar i)))
	a))))

(defun .linear (a)
  (make-array (reduce #'* (array-dimensions a))
	      :element-type (array-element-type a)
	      :displaced-to a))

(defun .max (a)
  (reduce #'max (.linear a)))

(defun .min (a)
  (reduce #'min (.linear a)))

#+nil
(let ((w 1600) ;; export yuv component images
      (h 1200)) 
 (let* ((y (make-array (list h w) :element-type '(unsigned-byte 8))))
   (dotimes (j h)
     (dotimes (i w)
       (setf (aref y j i) (aref *q* j i 0))))
   (write-pgm "/dev/shm/y.pgm" y)
   (format t "y ~a~%" (list (.min y) (.max y)))
   (let* ((w (/ w 2))
	  (u (make-array (list h w) :element-type '(unsigned-byte 8)))
	  (v (make-array (list h w) :element-type '(unsigned-byte 8)))
	  (r (make-array (list h w) :element-type '(unsigned-byte 8))))
     (dotimes (j h)
       (dotimes (i w)
	 (setf (aref u j i) (aref *q* j (* 2 i) 1))))
     (write-pgm "/dev/shm/u.pgm" u)
     (format t "u ~a~%" (list (.min u) (.max u)))
     (dotimes (j h)
       (dotimes (i w)
	 (setf (aref v j i) (aref *q* j (+ 1 (* 2 i)) 1))))
     (write-pgm "/dev/shm/v.pgm" v)
     (format t "v ~a~%" (list (.min v) (.max v)))
     (dotimes (j h)
       (dotimes (i w)
	 (setf (aref r j i) (min 255 (max 0 (floor (+ (aref y j (* 2 i)) 
					      (* 1.5748 (+ -128 (aref v j i))))))))))
     (write-pgm "/dev/shm/r.pgm" r))))

#+nil
(progn ;; export rgb component images
 (let* ((w 800)
	(h 600)
	(b (make-array (list h w) :element-type '(unsigned-byte 8))))
   (progn
    (dotimes (j h)
      (dotimes (i w)
	(setf (aref b j i) (aref *q* j i 0))))
    (write-pgm "/dev/shm/r.pgm" b))
   (progn
     (dotimes (j h)
       (dotimes (i w)
	 (setf (aref b j i) (aref *q* j i 1))))
     (write-pgm "/dev/shm/g.pgm" b))
   (progn
     (dotimes (j h)
       (dotimes (i w)
	 (setf (aref b j i) (aref *q* j i 2))))
     (write-pgm "/dev/shm/b.pgm" b))))


(defun write-pgm (fn a)
  (destructuring-bind (h w) (array-dimensions a) 
    (with-open-file (s fn :direction :output
                       :if-exists  :supersede
                       :if-does-not-exist :create)
      (format s "P5~%~a ~a~%255~%" w h))
    (with-open-file (s fn :direction :output
                       :if-exists  :append
                       :if-does-not-exist :create
                       :element-type '(unsigned-byte 8))
      (dotimes (j h)
        (dotimes (i w)
         (write-byte (aref a j i) s))))))
