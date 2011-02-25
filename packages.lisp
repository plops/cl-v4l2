(defpackage :v4l2-grovel
  (:use :cl :asdf :sb-grovel :sb-alien))

(defpackage :v4l2
  (:use :cl :v4l2-grovel))