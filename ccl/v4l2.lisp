#.(use-interface-dir :v4l2)

(progn
  (defvar *v4l-fd* nil)
  (defun v4l-open (&optional (fn "/dev/video1"))
    (unless *v4l-fd*
      (setf *v4l-fd* (with-cstrs ((fns fn))
		       (#_open fns (logior #$O_RDWR
					   #$O_NONBLOCK))))))
  (defun v4l-close ()
    (when *v4l-fd*
      (#_close *v4l-fd*))
    (setf *v4l-fd* nil))

  (defvar *buffers* nil)
  (defun v4l-allocate-buffers (&key (w 1600) (h 1200) (bytes-per-pixel 2))
    (unless *buffers*
      (setf *buffers* (let* ((number-buffers 4)
			     (length (* w h bytes-per-pixel))
			     (n (* number-buffers length))) 
			(multiple-value-bind (a ap) (make-heap-ivector n '(unsigned-byte 8))
			  (list ap length a (make-array (list number-buffers h w bytes-per-pixel)
							:element-type '(unsigned-byte 8)
							:displaced-to a)))))))
  (defun v4l-clear-buffers ()
    (when *buffers*
      (destructuring-bind (ap length a a4) *buffers*
	(dispose-heap-ivector a))
      (setf *buffers* nil)))
  (defun v4l-queue-buffers ()
    (destructuring-bind (ap length a a4) *buffers*
      (destructuring-bind (k h w c) (array-dimensions a4)
	(loop for i below k collect
	     (rletz ((buf :v4l2_buffer
			  :type #$V4L2_BUF_TYPE_VIDEO_CAPTURE
			  :memory #$V4L2_MEMORY_USERPTR
			  :index i
			  :m.userptr (+ (* i length) (%ptr-to-int ap))
			  :length length))
	       (let ((r (#_ioctl *v4l-fd* #$VIDIOC_QBUF :address buf)))
		 (assert (= 0 r))
		 r))))))
  (defun v4l-stream-on ()
    (multiple-value-bind (a ap) (make-heap-ivector 1 '(signed-byte 64))
      (setf (aref a 0) #$V4L2_BUF_TYPE_VIDEO_CAPTURE)
      (assert (= 0 (#_ioctl *v4l-fd* #$VIDIOC_STREAMON :address ap)))
      (dispose-heap-ivector a)))
  (defun v4l-stream-off ()
    (multiple-value-bind (a ap) (make-heap-ivector 1 '(signed-byte 64))
      (setf (aref a 0) #$V4L2_BUF_TYPE_VIDEO_CAPTURE)
      (assert (= 0 (#_ioctl *v4l-fd* #$VIDIOC_STREAMOFF :address ap)))
      (dispose-heap-ivector a)))
  (defun v4l-set-format (&key (w 1600) (h 1200))
    (rletz ((f :v4l2_format
	       :type #$V4L2_BUF_TYPE_VIDEO_CAPTURE
	       :fmt.pix.width w
	       :fmt.pix.height h
	       :fmt.pix.pixelformat #$V4L2_PIX_FMT_YUYV))
      (assert (= 0 (#_ioctl *v4l-fd* #$VIDIOC_S_FMT :address f)))
      (assert (= 0 (#_ioctl *v4l-fd* #$VIDIOC_G_FMT :address f)))
      (list (pref f :v4l2_format.fmt.pix.width)
	    (pref f :v4l2_format.fmt.pix.height)
	    :yuyv (= #$V4L2_PIX_FMT_YUYV
		     (pref f :v4l2_format.fmt.pix.pixelformat)))))
  (defun v4l-switch-to-user-pointers ()
    (rletz ((req :v4l2_requestbuffers ;; i want to read data with user pointers
		 :count 4
		 :type #$V4L2_BUF_TYPE_VIDEO_CAPTURE
		 :memory #$V4L2_MEMORY_USERPTR))
      (assert (= 0 (#_ioctl *v4l-fd* #$VIDIOC_REQBUFS :address req)))))
  (defun v4l-init (&key (fn "/dev/video1") (w 640) (h 480))
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
  (defun read-frame ()
    (rletz ((buf :v4l2_buffer
		 :type #$V4L2_BUF_TYPE_VIDEO_CAPTURE
		 :memory #$V4L2_MEMORY_USERPTR))
      (assert (= 0 (#_ioctl *v4l-fd* #$VIDIOC_DQBUF :address buf)))
      (prog1 
	  (destructuring-bind (ap length a a4) *buffers*
	    (floor (- (pref buf :v4l2_buffer.m.userptr) (%ptr-to-int ap))
		   length))
	(assert (= 0 (#_ioctl *v4l-fd* #$VIDIOC_QBUF :address buf))))))
  (defun wait-and-read-frame ()
    (%stack-block ((fdset ccl::*fd-set-size*))
      (ccl::fd-zero fdset)
      (ccl::fd-set *v4l-fd* fdset)
      (rletz ((tv :timeval
		  :tv_sec 2
		  :tv_usec 0))
	(tagbody
	 :again
	 (let ((r (#_select (+ 1 *v4l-fd*) fdset 
			    (%null-ptr) (%null-ptr) tv)))
	   (ecase r
	     (1 (return-from wait-and-read-frame (read-frame)))
	     (0 (break "error: select timed out."))
	     (-1 (break "error: select returned ~a."
			(let ((err (ccl::%get-errno)))
			  (if (= err (- #$EINTR))
			      (go :again)
			      (list err (ccl::%strerror err)))))))))))))


#+nil
(v4l-uninit)
#+nil
(v4l-init :fn "/dev/video1" :w 800 :h 600)
#+nil
(unless *v4l-fd*
  (v4l-open "/dev/video0"))
#+nil
(v4l-set-format :w 640 :h 640)
#+nil
(progn (v4l-stream-off)
       (v4l-stream-on)
       (wait-and-read-frame))
#+nil
(progn
  (v4l-init :fn "/dev/video0")
  (loop for i below 10 collect (wait-and-read-frame)))

#+nil
(wait-and-read-frame)
#+nil
(capture-and-copy-frame)

(defun capture-and-copy-frame ()
  (let* ((k (wait-and-read-frame))
	 (b1 (make-array (* 512 512) :element-type '(unsigned-byte 16)))
	 (b (make-array (list 512 512) :element-type '(unsigned-byte 16)
			:displaced-to b1)))
    (declare (type (array (unsigned-byte 8) 2) b)
	     (type (array (unsigned-byte 8) 1) b1))
    (when k
      (destructuring-bind  (ap length a a4) *buffers*
	(declare (type (array (unsigned-byte 8) 1) a)
		 (type (array (unsigned-byte 8) 4) a4))
	(destructuring-bind (kmax h w c) (array-dimensions a4)
	  (dotimes (i (min 512 w))
	    (dotimes (j (min 512 h))
	      (setf (aref b j i) (aref a4 k (* 1 j) (* 1 i) 0))))))
      (defparameter *bla* b))))
