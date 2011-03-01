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
(= v4l2::video-capture (logand v4l2::video-capture (v4l2::capability-capabilities *cap*)))
#+nil
(sb-posix:close *fd*)