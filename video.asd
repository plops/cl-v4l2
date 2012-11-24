(defpackage #:video-system (:use #:asdf #:cl))
(in-package #:video-system)

(asdf:defsystem :video
  :depends-on (:v4l2)
  :components ((:file "packages")
	       (:file "video")))
