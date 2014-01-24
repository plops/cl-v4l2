(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem :v4l2
    :depends-on (cffi iolib)
    :serial t
    :components
    ((:file "packages")
     (cffi-grovel:grovel-file "constants")
     (:file "video")))

