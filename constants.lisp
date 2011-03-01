("linux/videodev2.h")
#.(labels ((minus->underscore (seq)
	   (substitute #\_ #\- seq))
	 (make-slot (type c-type slots)
	   (loop for e in slots collect
		(etypecase e
		  (cons
		   (destructuring-bind (array-symb name) e
		     (unless (eq array-symb 'array) 
		       (error "I don't understand ~a. Its not of the form (array ...)." e))
		     `((array ,type) ,name ,c-type 
			  ,(string-downcase (minus->underscore (symbol-name name))))))
		  (symbol
		   `(,type ,e ,c-type ,(string-downcase (symbol-name e)))))))
	 (u32 (slots) (make-slot 'u32 "__u32" slots))
	 (u8 (slots)  (make-slot 'u8 "__u8" slots))
	 (constants-combine (prefix suffixes)
	   (loop for e in suffixes collect
		`(:integer ,e ,(concatenate 
				'string prefix 
				(string-upcase (minus->underscore (symbol-name e)))))))) 
  `(,@(loop for i in '((query-capability querycap)
		       (set-format s_fmt)
		       (get-format g_fmt)
		       qbuf
		       dqbuf streamon streamoff
		       reqbufs querybuf)
	 collect
	   `(:integer 
	     ,(intern (format nil "IO-~a" (if (consp i)
					      (first i)
					      i)))
	     ,(format nil "VIDIOC_~a" (if (consp i)
					  (second i)
					  i)))) 
      ,@(constants-combine "V4L2_PIX_FMT_"  '(RGB32 BGR32 RGB24 BGR24
					      GREY YUYV UYVY YUV422P))
      ,@(constants-combine "V4L2_CAP_"  '(video-capture video-output video-overlay vbi-capture
					  vbi-output sliced-vbi-capture sliced-vbi-output rds-capture
					  video-output-overlay hw-freq-seek rds-output tuner audio
					  radio modulator readwrite asyncio streaming))
      (:type u32 "__u32")
      (:type u8 "__u8")
      (:enum buf-type ((video-capture "V4L2_BUF_TYPE_VIDEO_CAPTURE")
		       (video-output "V4L2_BUF_TYPE_VIDEO_OUTPUT")))
      (:enum memory ((memory-mmap "V4L2_MEMORY_MMAP")
		     (memory-user-pointer "V4L2_MEMORY_USERPTR")
		     (memory-overlay "V4L2_MEMORY_OVERLAY")))
      (:structure request-buffers ("struct v4l2_requestbuffers"
				   ,@(u32 '(count))
				   (buf-type type "enum v4l2_buf_type" "type")
				   (memory memory "enum v4l2_memory" "memory")))
      (:structure buffer ("struct v4l2_buffer"
			  ,@(u32 '(index bytesused flags sequence length))))
      (:structure pix-format 
		  ("struct v4l2_pix_format"
		   ,@(u32 '(width height pixelformat bytesperline sizeimage))))
      (:structure format ("struct v4l2_format"
			  (buf-type type "v4l2_buf_type" "type")
			  (pix-format pix "v4l2_pix_format" "fmt.pix")))
      (:structure capability ("struct v4l2_capability"
			      ,@(u8 '((array driver) (array card) (array bus-info)))
			      ,@(u32 '(version capabilities))))))
