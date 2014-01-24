(in-package :v4l2)
(include "linux/videodev2.h" "linux/ioctl.h")

#+nil
(loop for e in '(V4L2_BUF_TYPE_VIDEO_CAPTURE
		 V4L2_MEMORY_USERPTR
		 VIDIOC_QBUF
		 V4L2_BUF_TYPE_VIDEO_CAPTURE
		 VIDIOC_STREAMON
		 VIDIOC_STREAMOFF
		 V4L2_PIX_FMT_YUYV
		 V4L2_PIX_FMT_RGB24
		 VIDIOC_S_FMT
		 VIDIOC_G_FMT
		 VIDIOC_REQBUFS
		 VIDIOC_DQBUF
		 VIDIOC_QBUF
		 EINTR)
   collect
     `(constant (,e ,(format nil "~a" e))))

(cenum (v4l2_buf_type :define-constants t) ((:capture "V4L2_BUF_TYPE_VIDEO_CAPTURE")))
(cenum (v4l2_memory :define-constants t) ((:userptr "V4L2_MEMORY_USERPTR")))
(CONSTANT (VIDIOC_QBUF "VIDIOC_QBUF"))
(CONSTANT (VIDIOC_STREAMON "VIDIOC_STREAMON"))
(CONSTANT (VIDIOC_STREAMOFF "VIDIOC_STREAMOFF"))
(CONSTANT (V4L2_PIX_FMT_YUYV "V4L2_PIX_FMT_YUYV"))
(CONSTANT (V4L2_PIX_FMT_RGB24 "V4L2_PIX_FMT_RGB24"))
(CONSTANT (VIDIOC_S_FMT "VIDIOC_S_FMT"))
(CONSTANT (VIDIOC_G_FMT "VIDIOC_G_FMT"))
(CONSTANT (VIDIOC_REQBUFS "VIDIOC_REQBUFS"))
(CONSTANT (VIDIOC_DQBUF "VIDIOC_DQBUF"))
(CONSTANT (VIDIOC_QBUF "VIDIOC_QBUF"))
;(CONSTANT (EINTR "EINTR"))


(cstruct v4l2_buffer "struct v4l2_buffer"
	 (type "type" :type :int)
	 (memory "memory" :type :int)
	 (index "index" :type :uint32)
	 (m.userptr "m.userptr" :type :ulong) ;; FIXME this valid?
	 (length "length" :type :uint32))

(cstruct v4l2_format "struct v4l2_format"
	 (type "type" :type :int)
	 (fmt.pix.width "fmt.pix.width" :type :uint32)
	 (fmt.pix.height "fmt.pix.height" :type :uint32)
	 (fmt.pix.pixelformat "fmt.pix.pixelformat" :type :uint32))

;; :v4l2_format
;; :type #$V4L2_BUF_TYPE_VIDEO_CAPTURE
;; :fmt.pix.width w
;; :fmt.pix.height h
;; :fmt.pix.pixelformat #$V4L2_PIX_FMT_YUYV


(cstruct v4l2_requestbuffers "struct v4l2_requestbuffers"
	 (count "count" :type :uint32)
	 (type "type" :type :int #+nil (enum v4l2_buf_type))
	 (memory "memory" :type :int #+nil (enum v4l2_memory)))

;; :v4l2_requestbuffers ;; i want to read data with user pointers
;; :count 4
;; :type #$V4L2_BUF_TYPE_VIDEO_CAPTURE
;; :memory #$V4L2_MEMORY_USERPTR
