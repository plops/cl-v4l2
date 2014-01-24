#+nil
(ql:quickload :ltk)
#+nil
(setf asdf:*central-registry*
        (union (list *default-pathname-defaults*)
               asdf:*central-registry*))

(require :v4l2)
(require :video)
(in-package :ltk)


#+nil
(with-ltk ()
  (let* ((bar (make-instance 'frame))
	 (b (make-instance 'button 
			   :master bar
			   :text "Press me"
			   :command (lambda ()
				      (format t  "Hello"))))
	 (combo (make-instance 'combobox :master bar :text "foo" :values '(foo bar baz)))
	 (fscale (make-instance 'frame :master bar))
	 (progress (make-instance 'progressbar :master fscale :value 0 :length 150))
	 (scale (make-instance 'scale :master fscale :from 0 :to 1  :length 150
			       :command (lambda (x) (set-control v4l2::gamma :relative x))))
	 (scale2 (make-instance 'scale :master fscale :from 0 :to 1  :length 150
			       :command (lambda (x) (set-control v4l2::brightness :relative x))))
	 (scale3 (make-instance 'scale :master fscale :from 0 :to 1  :length 150
			       :command (lambda (x) (set-control v4l2::contrast :relative x))))
	 (scale4 (make-instance 'scale :master fscale :from 0 :to 1  :length 150
				:command (lambda (x) (set-control v4l2::saturation :relative x))))
	 (separator (make-instance 'separator :master fscale))
	 
	 (fcheck (make-instance 'frame :master bar))
	 (ch1 (make-instance 'check-button :master fcheck :text "Salt")))
    (setf (value progress) 10)
    (configure scale :orient :horizontal)
    (bind *tk* "<Alt-q>" (lambda (event) 
			   (declare (ignore event))
			   (setf *exit-mainloop* t)))
    (pack bar :side :bottom)
    (pack b :side :top)
    (pack combo)
    (pack fscale)
    (pack progress)
    (pack scale)
    (pack scale2)

    (pack scale3)

    (pack scale4)
    (pack separator)
    (pack fcheck)
    (pack ch1)))

#+nil
(ltk::ltktest)
