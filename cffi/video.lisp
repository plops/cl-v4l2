(in-package :v4l2)
(progn
  (defparameter *capture* 1)
  (defparameter *userptr* 2)
  (defvar *v4l-fd* nil)
  (defun v4l-open (&optional (fn "/dev/video0"))
    (unless *v4l-fd*
      (setf *v4l-fd*
	    (iolib.syscalls:open fn
				 (logior iolib.syscalls:o-rdwr
					 iolib.syscalls:o-nonblock)))))
  (defun v4l-close ()
    (when *v4l-fd*
      (iolib.syscalls:close *v4l-fd*)
      (setf *v4l-fd* nil)))

  (defvar *buffers* nil)
  (defun v4l-allocate-buffers (&key (w 800) (h 600) (bytes-per-pixel 2))
    (if *buffers*
	(break "buffers are already allocated.")
	(setf *buffers* (let* ((number-buffers 4)
			       (length (* w h bytes-per-pixel))
			       (n (* number-buffers length))) 
			  (list (cffi:foreign-alloc :uchar :count n) number-buffers length)))))
  (defun v4l-clear-buffers ()
    (when *buffers*
      (destructuring-bind (ap n length) *buffers*
	(declare (ignorable n length))
	(cffi:foreign-free ap))
      (setf *buffers* nil)))
  (defun v4l-set-format (&key (w 800) (h 600))
    (cffi:with-foreign-object (f '(:struct v4l2_format))
      (setf (cffi:foreign-slot-value f '(:struct v4l2_format) 'type) *capture*)
      (setf (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.width) w)
      (setf (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.height) h)
      (setf (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.pixelformat)
	    V4L2_PIX_FMT_RGB24)
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_S_FMT f)))
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_G_FMT f)))
      (list (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.width)
	    (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.height)
	    (= V4L2_PIX_FMT_RGB24
	       (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.pixelformat)))))
  (defun v4l-switch-to-user-pointers ()
    (cffi:with-foreign-object (f '(:struct v4l2_requestbuffers))
      (setf (cffi:foreign-slot-value f '(:struct v4l2_requestbuffers) 'count) 4
	    (cffi:foreign-slot-value f '(:struct v4l2_requestbuffers) 'type) *capture*
	    (cffi:foreign-slot-value f '(:struct v4l2_requestbuffers) 'memory) *userptr*)
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_REQBUFS f)))))
  (defun v4l-queue-buffers ()
    (destructuring-bind (ap n length) *buffers*
      (declare (ignorable ap))
      (loop for i below n collect
	   (cffi:with-foreign-object (buf '(:struct v4l2_buffer))
	     (setf (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'type) *capture*)
	     (setf (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'memory) *userptr*)
	     (setf (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'index) i)
	     (setf (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'm.userptr)
		   (cffi:pointer-address (cffi:inc-pointer ap (* i length))))
	     (setf (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'length) length)
	     (let ((r (iolib.syscalls:ioctl *v4l-fd* VIDIOC_QBUF buf)))
	       (assert (= 0 r))
	       r)))))
  (defun v4l-stream-on ()
    (cffi:with-foreign-object (a :int)
      (setf (cffi:mem-ref a :int) *capture*)
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_STREAMON a)))))
  (defun v4l-stream-off ()
    (cffi:with-foreign-object (a :int)
      (setf (cffi:mem-ref a :int) *capture*)
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_STREAMOFF a)))))
  (defun read-frame ()
    (cffi:with-foreign-object (buf '(:struct v4l2_buffer))
      (setf (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'type) *capture*)
      (setf (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'memory) *userptr*)
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_DQBUF buf)))
      (prog1
	  (destructuring-bind (a n length) *buffers*
	    (declare (ignorable n))
	    (floor (- (cffi:foreign-slot-value buf '(:struct v4l2_buffer) 'm.userptr)
		      (cffi:pointer-address a))
		   length))
	(assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_QBUF buf))))
      ))
  (defun wait-and-read-frame ()
    (cffi:with-foreign-objects ((fdset '(:struct iolib.syscalls:fd-set))
				(tv '(:struct iolib.syscalls:timeval)))
      (iolib.syscalls:fd-zero fdset)
      (iolib.syscalls:fd-set *v4l-fd* fdset)
      (setf (cffi:foreign-slot-value tv '(:struct iolib.syscalls:timeval) 'iolib.syscalls:sec) 2
	    (cffi:foreign-slot-value tv '(:struct iolib.syscalls:timeval) 'iolib.syscalls:usec) 0)
      (tagbody
       :again
	 (let ((r (iolib.syscalls:select (+ 1 *v4l-fd*) fdset
					 (cffi:null-pointer) (cffi:null-pointer)
					 tv)))
	   (ecase r
	     (1 (return-from wait-and-read-frame (read-frame)))
	     (0 (break "error: select timed out."))
	     (-1 (break "error: select returnd ~a."
			(if (= (isys:errno) isys:eintr)
			    (go :again)
			    (isys:strerror)))))))))
  (defun v4l-init (&key (fn "/dev/video0") (w 640) (h 480))
    (unless *v4l-fd*
      (v4l-open fn))
    (v4l-set-format :w w :h h)
    (v4l-switch-to-user-pointers)
    
    (v4l-clear-buffers)
    (v4l-allocate-buffers :w w :h h)
    (v4l-queue-buffers)
    (v4l-stream-on))

  (defun v4l-uninit ()
    (v4l-stream-off)
    (v4l-clear-buffers)
    (v4l-close))
  )
