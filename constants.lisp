("linux/videodev2.h")
((:integer s-fmt "VIDIOC_S_FMT")
 (:integer rgb24 "V4L2_PIX_FMT_RGB24")
 (:integer bgr24 "V4L2_PIX_FMT_BGR24")
 (:integer rgb32 "V4L2_PIX_FMT_RGB32")
 (:integer grey "V4L2_PIX_FMT_GREY")
 (:integer yuyv "V4L2_PIX_FMT_YUYV")
 (:integer uyvy "V4L2_PIX_FMT_UYVY")
 (:integer yuv422p "V4L2_PIX_FMT_YUV422P")
 
 (:type u32 "__u32")
 (:enum buf-type ((video-capture "V4L2_BUF_TYPE_VIDEO_CAPTURE")
		       (video-output "V4L2_BUF_TYPE_VIDEO_OUTPUT")))
 (:structure buffer #.`("struct v4l2_buffer"
		     ,@(loop for i in '("index" "bytesused" "flags" "sequence"
					"length") collect
			    `(u32 ,(intern (string-upcase i)) "__u32" ,i))))
 (:structure pix-format 
	     #.`("struct v4l2_pix_format"
	      ,@(loop for i in '("width" "height" "pixelformat" 
				 "bytesperline" "sizeimage")
		   collect
		     `(u32 ,(intern (string-upcase i)) "__u32" ,i))))
 (:structure format ("struct v4l2_format"
		     (buf-type type "v4l2_buf_type" "type"))))