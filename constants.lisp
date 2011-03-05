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
		      `(,type ,e ,c-type ,(minus->underscore (string-downcase (symbol-name e))))))))
	    (u32 (slots) (make-slot 'u32 "__u32" slots))
	    (s32 (slots) (make-slot 's32 "__s32" slots))
	    (u8 (slots)  (make-slot 'u8 "__u8" slots))
	    (constants-combine (prefix suffixes)
	      (loop for e in suffixes collect
		   `(:integer ,e ,(concatenate 
				   'string prefix 
				   (string-upcase (minus->underscore (symbol-name e)))))))
	    (enum-combine (prefix suffixes)
	      (loop for e in suffixes collect
		   (destructuring-bind (lisp-name c-name)
		       (if (consp e)
			   e
			   (list e e))
		     `(,lisp-name ,(concatenate 
				 'string prefix 
				 (string-upcase (minus->underscore (symbol-name c-name))))))))) 
     `(,@(loop for i in '((query-capability querycap)
			  (set-format s_fmt)
			  (get-format g_fmt)
			  qbuf
			  dqbuf streamon streamoff
			  reqbufs querybuf
			  queryctl (get-control g_ctrl) (set-control s_ctrl))
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
	 (:type s32 "__s32")
	 (:type u8 "__u8")
	 (:enum buf-type ,(enum-combine "V4L2_BUF_TYPE_"
					'(video-capture video-output)))
	 (:enum power-line-frequency 
		,(enum-combine "V4L2_CID_POWER_LINE_"
			       '(frequency-disabled frequency-50Hz frequency-60Hz)))
	 (:enum memory
		,(enum-combine "V4L2_"
			       '(memory-mmap (memory-user-pointer memory-userptr) memory-overlay)))
	 (:structure request-buffers ("struct v4l2_requestbuffers"
				      ,@(u32 '(count))
				      (buf-type type "enum v4l2_buf_type" "type")
				      (memory memory "enum v4l2_memory" "memory")))
	 (:structure buffer ("struct v4l2_buffer"
			     (buf-type type "enum v4l2_buf_type" "type")
			     (memory memory "enum v4l2_memory" "memory")
			     (u32 offset "__u32" "m.offset")
			     ,@(u32 '(index bytesused flags sequence length))))
	 (:structure pix-format 
		     ("struct v4l2_pix_format"
		      ,@(u32 '(width height pixelformat bytesperline sizeimage))))
	 (:structure format ("struct v4l2_format"
			     (buf-type type "v4l2_buf_type" "type")
			     (pix-format pix "v4l2_pix_format" "fmt.pix")))
	 (:structure capability ("struct v4l2_capability"
				 ,@(u8 '((array driver) (array card) (array bus-info)))
				 ,@(u32 '(version capabilities))))
	 ,@(constants-combine "V4L2_CID_"  '(base lastp1 brightness contrast saturation hue
					     auto-white-balance red-balance blue-balance gamma
					     exposure autogain gain hflip vflip
					     power-line-frequency hue-auto white-balance-temperature
					     sharpness backlight-compensation chroma-agc color-killer
					     autobrightness band-stop-filter))
	 (:enum query-ctrl-type ,(enum-combine "V4L2_CTRL_TYPE_"
					       '(integer boolean menu button integer64
						 ctrl-class string)))
	 ,@(constants-combine "V4L2_CTRL_" '(flag-disabled flag-grabbed flag-read-only flag-update
					     flag-inactive flag-slider flag-write-only flag-next-ctrl))
	 (:structure query-control ("struct v4l2_queryctrl"
				    ,@(u8 '((array name)))
				    ,@(u32 '(id flags))
				    (query-ctrl-type type "enum v4l2_ctrl_type" "type")
				    ,@(s32 '(minimum maximum step default-value))))))
