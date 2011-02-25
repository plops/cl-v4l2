("linux/videodev2.h")


#.`((:integer s-fmt "VIDIOC_S_FMT")
    ,@(loop for i in '("RGB32" "BGR32" "RGB24" "BGR24"
		       "GREY" "YUYV" "UYVY" "YUV422P")
	 collect
	   `(:integer ,(intern i) 
		      ,(concatenate 'string "V4L2_PIX_FMT_" i)))
    (:type u32 "__u32")
    (:type u8 "__u8")
    (:enum buf-type ((video-capture "V4L2_BUF_TYPE_VIDEO_CAPTURE")
		     (video-output "V4L2_BUF_TYPE_VIDEO_OUTPUT")))
    (:structure buffer ("struct v4l2_buffer"
			,@(loop for i in '("index" "bytesused" "flags" "sequence"
					   "length") collect
			       `(u32 ,(intern (string-upcase i)) "__u32" ,i))))
    (:structure pix-format 
		("struct v4l2_pix_format"
		 ,@(loop for i in '("width" "height" "pixelformat" 
				    "bytesperline" "sizeimage")
		      collect
			`(u32 ,(intern (string-upcase i)) "__u32" ,i))))
    (:structure format ("struct v4l2_format"
			(buf-type type "v4l2_buf_type" "type"))))