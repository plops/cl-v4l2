#+nil
(setf asdf:*central-registry*
        (union (list *default-pathname-defaults*)
               asdf:*central-registry*))
#+nil
(asdf:operate 'asdf:load-op 'v4l2)

(defpackage :g
  (:use :cl :v4l2))

(in-package :g)

#+nil
(v4l-init :w 800 :h 600)
#+nil
(v4l-uninit)
#+nil
(defparameter *blub*
 (loop for i below 100 collect
      (wait-and-read-frame)))

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *q*
  (let ((r (first *blub*))
	(w 800)
	(h 600)
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

#+nil
(progn ;; export yuv component images
 (let* ((w 800)
	(h 600)
	(b (make-array (list h w) :element-type '(unsigned-byte 8))))
   (dotimes (j h)
     (dotimes (i w)
       (setf (aref b j i) (aref *q* j i 0))))
   (write-pgm "/dev/shm/y.pgm" b))

 (let* ((w 400)
	(h 600)
	(b (make-array (list h w) :element-type '(unsigned-byte 8))))
   (dotimes (j h)
     (dotimes (i w)
       (setf (aref b j i) (aref *q* j (* 2 i) 1))))
   (write-pgm "/dev/shm/u.pgm" b)
   (dotimes (j h)
     (dotimes (i w)
       (setf (aref b j i) (aref *q* j (+ 1 (* 2 i)) 1))))
   (write-pgm "/dev/shm/v.pgm" b)))


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
