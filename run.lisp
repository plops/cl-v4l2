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

#+nil
(defparameter *fd* (sb-posix:open "/dev/video0" sb-posix:o-rdwr))
#+nil
(defparameter *cap* (v4l2::allocate-capability))
#+nil
(sb-posix:ioctl *fd* v4l2::io-query-capability *cap*)
#+nil
(defparameter *card* (v4l2::capability-card *cap*))
#+nil
(remove-if-not #'second (parse-capabilities (v4l2::capability-capabilities *cap*)))

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
   (/= -1 (sb-posix:ioctl fd v4l2::io-reqbufs rb))))

(defun supports-user-pointerp (fd)
 (let ((rb (v4l2::allocate-request-buffers)))
   (setf (v4l2::request-buffers-type rb) 'v4l2::video-capture
	 (v4l2::request-buffers-memory rb) 'v4l2::memory-user-pointer)
   (/= -1 (sb-posix:ioctl fd v4l2::io-reqbufs rb))))

#+nil
(supports-user-pointer-p *fd*)
#+nil
(supports-mmap-p *fd*)

(defun init-mmap (fd &optional (count 4))
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
		      len)
		res))))
    (reverse res)))


(defun uninit-mmap (bufs)
  (loop for (start len) in bufs do
       (sb-posix:munmap start len)))

#+nil
(defparameter *bufs* (init-mmap *fd*))

#+nil
(uninit-mmap *bufs*)

#+nil
(/ (v4l2::buffer-length *b*) 512)
#+nil
(v4l2::buffer-offset *b*)

#+nil
(sb-posix:mmap (sb-sys:int-sap 0)
	       (v4l2::buffer-length *b*) 
	       (logior sb-posix:prot-read
		       sb-posix:prot-write)
	       sb-posix:map-shared
	       *fd*
	       (v4l2::buffer-offset *b*))