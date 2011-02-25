(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :sb-grovel))
(defpackage #:v4l2-system (:use #:asdf #:cl #:sb-grovel))
(in-package #:v4l2-system)

(asdf:defsystem :v4l2
  :depends-on (sb-grovel)
  :components ((:file "packages")
	       (sb-grovel:grovel-constants-file "constants"
						:package :v4l2
						:depends-on ("packages"))))