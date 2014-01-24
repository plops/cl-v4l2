#+nil
(setf asdf:*central-registry*
        (union (list *default-pathname-defaults*)
               asdf:*central-registry*))
(eval-when (:compile-toplevel :load-toplevel :execute) 
  (require :asdf)
  (require :v4l2)
  (require :sb-posix))

#+nil
(ql:quickload "iolib")

#+nil
(require :iolib)

#+nil
(setf asdf:*central-registry*
        (union (list *default-pathname-defaults*)
               asdf:*central-registry*))
#+nil
(asdf:operate 'asdf:load-op 'v4l2)

(in-package :v4l2)

;(cffi:defcvar "errno" :int)

#+nil
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
	    V4L2_PIX_FMT_YUYV)
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_S_FMT f)))
      (assert (= 0 (iolib.syscalls:ioctl *v4l-fd* VIDIOC_G_FMT f)))
      (list (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.width)
	    (cffi:foreign-slot-value f '(:struct v4l2_format) 'fmt.pix.height)
	    (= V4L2_PIX_FMT_YUYV
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

#+nil
(v4l-open)
#+nil
(v4l-clear-buffers)
#+nil
(v4l-allocate-buffers :w 640 :h 480) 
#+nil
(v4l-set-format :w 640 :h 480)
#+nil
(v4l-switch-to-user-pointers)
#+nil
(v4l-queue-buffers)
#+nil
(v4l-stream-on)
#+nil
(loop for i below 100 collect
 (wait-and-read-frame))

(in-package :video)


#+nil
(query-controls *fd*)
#+nil
(set-control v4l2::gamma :relative 1s0)
#+nil
(set-controls)

(defmacro parse-capabilities (cap)
  (let* ((caps '(video-capture video-output video-overlay vbi-capture
		 vbi-output sliced-vbi-capture sliced-vbi-output rds-capture
		 video-output-overlay hw-freq-seek rds-output tuner audio
		 radio modulator readwrite asyncio streaming))
	 (vcaps (loop for e in caps collect
		     (intern (symbol-name e) 'v4l2))))
    `(list ,@(loop for e in vcaps 
	   collect
		  `(list ',e (= ,e (logand ,cap ,e)))))))

(defvar *fd* nil)
#+nil
(defvar *fd* (sb-posix:open "/dev/video0" sb-posix:o-rdwr))

(defun list-capabilities (fd)
  (let ((cap (v4l2::allocate-capability)))
    (unwind-protect
	 (progn
	   (sb-posix:ioctl fd v4l2::io-query-capability cap)
	   (return-from list-capabilities 
	     (parse-capabilities 
	      (v4l2::capability-capabilities cap))))
      (v4l2::free-capability cap))))

#+nil
(list-capabilities *fd*)

(defun supports-streaming-p (fd)
  (let ((cap (v4l2::allocate-capability)))
    (sb-posix:ioctl fd v4l2::io-query-capability cap)
    (prog1 (= v4l2::streaming 
	      (logand (v4l2::capability-capabilities cap)
		      v4l2::streaming))
      (v4l2::free-capability cap))))

#+nil
(supports-streaming-p *fd*)

#+nil
(sb-posix:close *fd*)


(defun supports-mmap-p (fd)
 (let ((rb (v4l2::allocate-request-buffers)))
   (setf (v4l2::request-buffers-type rb) 'v4l2::video-capture
	 (v4l2::request-buffers-memory rb) 'v4l2::memory-mmap)
   (prog1
       (/= -1 (sb-posix:ioctl fd v4l2::io-reqbufs rb))
     (v4l2::free-request-buffers rb))))

(defun supports-user-pointer-p (fd)
 (let ((rb (v4l2::allocate-request-buffers)))
   (setf (v4l2::request-buffers-type rb) 'v4l2::video-capture
	 (v4l2::request-buffers-memory rb) 'v4l2::memory-user-pointer)
   (prog1
       (/= -1 (sb-posix:ioctl fd v4l2::io-reqbufs rb))
     (v4l2::free-request-buffers rb))))

#+nil
(supports-user-pointer-p *fd*)
#+nil
(supports-mmap-p *fd*)

(defun init-mmap (fd &optional (count 30))
  "Allocate buffers for image storage with MMAP."
  (let ((res ())
	(rb (v4l2::allocate-request-buffers)))
    (setf (v4l2::request-buffers-type rb) 'v4l2::video-capture
	  (v4l2::request-buffers-memory rb) 'v4l2::memory-mmap
	  (v4l2::request-buffers-count rb) count)
    (when (= -1 (sb-posix:ioctl fd v4l2::io-reqbufs rb))
      (error "video capture or mmap streaming not supported."))
    (assert (<= count (v4l2::request-buffers-count rb)))
    (dotimes (i (v4l2::request-buffers-count rb))
      (let ((b (v4l2::allocate-buffer)))
	(setf (v4l2::buffer-type b) 'v4l2::video-capture
	      (v4l2::buffer-memory b) 'v4l2::memory-mmap
	      (v4l2::buffer-index b) i)
	(assert (/= -1 (sb-posix:ioctl fd v4l2::io-querybuf b)))
	(let ((len (v4l2::buffer-length b)))
	  (push (list (sb-posix:mmap (sb-sys:int-sap 0)
				     len
				     (logior sb-posix:prot-read
					     sb-posix:prot-write)
				     sb-posix:map-shared
				     fd
				     (v4l2::buffer-offset b))
		      len
		      i
		      nil)
		res))
	(v4l2::free-buffer b)))
    (v4l2::free-request-buffers rb)
    (reverse res)))

(defun uninit-mmap (bufs)
  (loop for (start len index is-queued) in bufs do
       (sb-posix:munmap start len)))

(defun set-format (fd &key (width 640) (height 480))
  (let* ((f (v4l2::allocate-format))
	 (p (sb-alien:slot f 'v4l2::pix)))
    (unwind-protect
	 (progn (setf (v4l2::format-type f) 'v4l2::video-capture
		      (sb-alien:slot p 'v4l2::width) width
		      (sb-alien:slot p 'v4l2::height) height
		      (sb-alien:slot p 'v4l2::pixelformat) v4l2::yuyv)
		;; make a format suggestion to the driver
		(sb-posix:ioctl fd v4l2::io-set-format f)
		(let ((p (sb-alien:slot f 'v4l2::pix)))
		  ;; return values that the driver has chosen
		  `((width ,(sb-alien:slot p 'v4l2::width))
		    (height ,(sb-alien:slot p 'v4l2::height)))))
      (v4l2::free-format f))))
#+nil
(set-format *fd* :width 1280 :height 720)

(defvar *bufs* nil)
#+nil
(setf *bufs* (init-mmap *fd*))

#+nil
(uninit-mmap *bufs*)

(defun enqueue (fd buf)
  (destructuring-bind (start len index is-queued) buf
    (declare (ignore start len))
    (unless is-queued
     (let ((b (v4l2::allocate-buffer)))
       (setf (v4l2::buffer-type b) 'v4l2::video-capture
	     (v4l2::buffer-memory b) 'v4l2::memory-mmap
	     (v4l2::buffer-index b) index)
       (when (= -1 (sb-posix:ioctl fd v4l2::io-qbuf b))
	 (break "qbuf error maybe buffer has been queued already?"))
       (setf (fourth buf) t)
       (v4l2::free-buffer b)))))

(defun exchange-queue (fd process-img)
  (unless fd
    (error "file descriptor isn't opened."))
  (unless *bufs*
    (error "no mmap buffers available."))
  (let ((b (v4l2::allocate-buffer)))
    (setf (v4l2::buffer-type b) 'v4l2::video-capture
	  (v4l2::buffer-memory b) 'v4l2::memory-mmap)
    (sb-posix:ioctl fd v4l2::io-dqbuf b)
    (funcall process-img (v4l2::buffer-index b))
    (sb-posix:ioctl fd v4l2::io-qbuf b)
    (v4l2::free-buffer b)))

(defun init (&key (fn "/dev/video0") (width 352) (height 288))
  (setf *fd* (sb-posix:open fn sb-posix:o-rdwr))
  (set-format *fd* :width width :height height)
  (setf *bufs* (init-mmap *fd*))
  #+nil (set-controls))


(defun uninit ()
  (uninit-mmap *bufs*)
  (setf *bufs* nil)
  (sb-posix:close *fd*)
  (setf *fd* nil))

(defun start-capturing ()
  (unless *bufs*
    (error "You forgot to call init-mmap."))
  (dolist (e *bufs*) 
    (enqueue *fd* e))
  (sb-alien:with-alien ((v sb-alien:integer :local v4l2::video-capture))
    (assert (/= -1 (sb-posix:ioctl *fd* 
				   v4l2::io-streamon
				   (sb-alien:addr v))))))

(defun stop-capturing ()
  (sb-alien:with-alien ((v sb-alien:integer :local v4l2::video-capture))
    (assert (/= -1 (sb-posix:ioctl *fd* 
				   v4l2::io-streamoff
				   (sb-alien:addr v))))))

#+nil
(stop-capturing)

(defun get-control (id)
  (let ((s (v4l2::allocate-control)))
    (unwind-protect
	 (progn
	   (setf (v4l2::control-id s) id)
	   (sb-posix:ioctl *fd* v4l2::io-get-control s)
	   (return-from get-control (v4l2::control-value s)))
      (v4l2::free-control s))))

#+nil
(get-control v4l2::brightness)

(defun query-controls (fd)
  (let ((c (v4l2::allocate-query-control))
	(res ()))
    (unwind-protect
	 (dolist (e '(			;base lastp1 
		      brightness contrast saturation hue
		      auto-white-balance
		      red-balance blue-balance
		      gamma
		      exposure autogain gain hflip vflip
		      power-line-frequency 
		      hue-auto
		      white-balance-temperature
		      sharpness backlight-compensation 
		      chroma-agc
		      color-killer
		      autobrightness
		      band-stop-filter
		      camera-class-base camera-class
		      exposure-auto
		      exposure-absolute
		      exposure-auto-priority
		      pan-relative tilt-relative
		      pan-reset tilt-reset
		      pan-absolute tilt-absolute
		      focus-absolute focus-relative focus-auto
		      zoom-absolute zoom-relative zoom-continuous
		      ))
	   (setf (v4l2::query-control-id c) (symbol-value
					     (intern (symbol-name e) 'v4l2)))
	   (handler-case 
	       (unless (or (= -1 (sb-posix:ioctl fd v4l2::io-query-control c)) 
			   (= v4l2::flag-disabled 
			      (logand v4l2::flag-disabled 
				      (v4l2::query-control-flags c))))
		 (push `(,e . ((value . ,(get-control (symbol-value
						       (intern (symbol-name e) 'v4l2))))
			       (type . ,(v4l2::query-control-type c))
			       (step . ,(v4l2::query-control-step c))
			       (range . 
				      ,(list
					(v4l2::query-control-minimum c)
					(v4l2::query-control-default-value c)
					(v4l2::query-control-maximum c))))) res))
	     (sb-posix:syscall-error nil)))
      (v4l2::free-query-control c))
    (reverse res)))
#+nil
(query-controls *fd*)

(defun set-control (id &key value relative)
  (declare (type (or null (single-float 0s0 1s0)) relative))
  (let ((g (v4l2::allocate-query-control))
	(s (v4l2::allocate-control)))
    (unwind-protect
	 (progn
	   (setf (v4l2::query-control-id g) id)
	   (sb-posix:ioctl *fd* v4l2::io-query-control g)
	   (unless (= v4l2::flag-disabled 
		      (logand v4l2::flag-disabled (v4l2::query-control-flags g)))
	     (let ((mi (v4l2::query-control-minimum g))
		   (ma (v4l2::query-control-maximum g))
		   (default (v4l2::query-control-default-value g)))
	       (setf (v4l2::control-id s) id
		     (v4l2::control-value s) 
		     (if value
			 (min ma (max mi value))
			 (if relative
			     (floor (+ (* ma relative)
				       (* (- 1 relative) mi)))
			     default)))
	       (sb-posix:ioctl *fd* v4l2::io-set-control s)
	       (return-from set-control (values
					 (v4l2::control-value s)
					 mi default ma)))))
      (progn (v4l2::free-query-control g)
	     (v4l2::free-control s)))))


(defvar *good-controls* 
  '((gamma max)
    (saturation min)
    (contrast max)
    (brightness max)
    (auto-white-balance 0)
    (power-line-frequency 0)
    (white-balance-temperature min)
    (sharpness 0)
    (backlight-compensation 0)
    (exposure-auto 1)
    (exposure-auto-priority 0)))
#+nil
(set-controls)

(defun set-controls (&optional (controls *good-controls*))
  (dolist (e controls)
    (destructuring-bind (sym val) e
      (let ((name (symbol-value
		   (intern (symbol-name sym) 'v4l2))))
	(etypecase val
	  (integer
	   (set-control name :value val))
	  (real
	   (set-control name :relative val))
	  (symbol
	   (ecase val
	     (max (set-control name :relative 1s0))
	     (min (set-control name :relative 0s0))
	     (default (set-control name)))))))))

#+NIL
(set-control v4l2::exposure-absolute :relative .2s0) ;; doesn't work

(defun start-main-loop (&key (fn "/dev/video0") (width 352) (height 288))
  (unwind-protect
       (progn 
	 (init :fn fn :width width :height height)
	 (start-capturing)
	 (time
	  (dotimes (i 100)
	    (exchange-queue *fd* #'(lambda (index)
				     (format t "~a~%"
					    (sb-sys:sap-ref-8
					     (first (elt *bufs* index))
					     0)))))))
    (stop-capturing)))


#+nil ;; only fast if there is enough light
(start-main-loop :fn "/dev/video1" :width 1600 :height 1200)
; uvcdynctrl -d /dev/video1 -s "Disable video processing" 1

#+nil
(uninit)


(defun parse-rect (r)
  (list (sb-alien:slot r 'v4l2::left)
	(sb-alien:slot r 'v4l2::top)
	(sb-alien:slot r 'v4l2::width)
	(sb-alien:slot r 'v4l2::height)))

(defmacro with-rect ((name rect-list) &body body)
  `(let ((,name (v4l2::allocate-rect)))
     (unwind-protect
	  (destructuring-bind (x y w h) ,rect-list  
	    (setf (sb-alien:slot ,name 'v4l2::left) x
		  (sb-alien:slot ,name 'v4l2::top) y
		  (sb-alien:slot ,name 'v4l2::width) w
		  (sb-alien:slot ,name 'v4l2::height) h)
	   ,@body)
       (v4l2::free-rect ,name))))

(defun set-rect (r x y w h)
  (setf (sb-alien:slot r 'v4l2::left) x
	(sb-alien:slot r 'v4l2::top) y
	(sb-alien:slot r 'v4l2::width) w
	(sb-alien:slot r 'v4l2::height) h))


(defun parse-fract (f)
  (list (sb-alien:slot f 'v4l2::denominator)
	(sb-alien:slot f 'v4l2::numerator)))

(defun list-cropcap (fd)
 (let ((c (v4l2::allocate-cropcap)))
   (unwind-protect
	(progn
	  (setf (v4l2::cropcap-type c) 'v4l2::video-capture)
	  (sb-posix:ioctl fd v4l2::io-cropcap c)
	  `((defrect ,(parse-rect (v4l2::cropcap-defrect c)))
	    (bounds ,(parse-rect (v4l2::cropcap-bounds c)))
	    (pixelaspect ,(parse-fract (v4l2::cropcap-pixelaspect c)))))
     (v4l2::free-cropcap c))))
#+nil
(first (cdr (assoc 'bounds
	     (list-cropcap *fd*))))

(defun set-crop (fd &optional (rect (first (cdr (assoc 'bounds
						       (list-cropcap *fd*))))))
 (let ((c (v4l2::allocate-crop)))
   (unwind-protect
	(destructuring-bind (x y w h) rect
	  (set-rect (v4l2::crop-c c) x y w h)
	  (setf (v4l2::crop-type c) 'v4l2::video-capture)
	  (sb-posix:ioctl fd v4l2::io-set-crop c)
	  `(c ,(parse-rect (v4l2::crop-c c))))
     (v4l2::free-cropcap c))))
#+nil
(set-crop *fd* '(200 200 210 210))
