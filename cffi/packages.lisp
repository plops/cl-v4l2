(defpackage :v4l2
  (:use :cl :iolib)
  (:shadowing-import-from :cl #:read #:open #:close #:time #:write #:truncate
			  #:ftruncate))

(defpackage :video
  (:use :cl)
  (:export
   #:list-capabilities
   #:*fd*
   #:supports-streaming-p
   #:supports-mmap-p
   #:supports-user-pointer-p
   #:set-format
   #:exchange-queue
   #:init
   #:uninit
   #:start-capturing
   #:stop-capturing
   #:get-control
   #:query-controls
   #:set-control
   #:set-controls
   #:start-main-loop))
