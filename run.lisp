(eval-when (:compile-toplevel :load-toplevel :execute) 
  (require :asdf)
  (require :v4l2)
  (require :sb-posix))

(defpackage :run
  (:use :cl :v4l2))

(in-package :run)

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

(defun set-format (fd)
  (let* ((f (v4l2::allocate-format))
	 (p (sb-alien:slot f 'v4l2::pix)))
    (setf (v4l2::format-type f) 'v4l2::video-capture
	  (sb-alien:slot p 'v4l2::width) 640
	  (sb-alien:slot p 'v4l2::height) 480
	  (sb-alien:slot p 'v4l2::pixelformat) v4l2::yuyv)
    (sb-posix:ioctl fd v4l2::io-set-format f)
    (v4l2::free-format f)))
#+nil
(set-format *fd*)

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

(defun init ()
  (setf *fd* (sb-posix:open "/dev/video0" sb-posix:o-rdwr))
  (set-format *fd*)
  (setf *bufs* (init-mmap *fd*)))


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
		 (push (list e
			     (get-control (symbol-value
					   (intern (symbol-name e) 'v4l2)))
			     (list
			      (v4l2::query-control-minimum c)
			      (v4l2::query-control-default-value c)
			      (v4l2::query-control-maximum c))) res))
	     (sb-posix:syscall-error nil)))
      (v4l2::free-query-control c))
    (reverse res)))
#+nil
(format t "~a~%"
 (query-controls *fd*))

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
(set-good-controls)

(defun set-good-controls ()
  (dolist (e *good-controls*)
    (destructuring-bind (sym val) e
      (let ((name (symbol-value
		   (intern (symbol-name sym) 'v4l2))))
	(etypecase val
	  (integer
	   (set-control name :value val))
	  (real
	   (set-control name :relative val))
	  (symbol
	   (set-control name :relative (ecase val
					 (max 1s0)
					 (min 0s0)))))))))

#+NIL
(set-control v4l2::exposure-absolute :relative .2s0) ;; doesn't work

(defun start-main-loop ()
  (unwind-protect
       (progn 
	 (init)
	 (start-capturing)
	 (time
	  (dotimes (i 100)
	    (exchange-queue *fd* #'(lambda (index)
				     (format t "~a~%"
					     (sb-sys:sap-ref-8 
					      (first (elt *bufs* index))
					      0)))))))
    (stop-capturing)))

#+nil
(start-main-loop)

#+nil
(uninit)