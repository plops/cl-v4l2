(defpackage :v4l2
  (:use :cl :iolib)
  (:shadowing-import-from :cl #:read #:open #:close #:time #:write #:truncate
			  #:ftruncate)
  (:export #:v4l-init
	   #:v4l-uninit
	   #:*buffers*
	   #:wait-and-read-frame))


