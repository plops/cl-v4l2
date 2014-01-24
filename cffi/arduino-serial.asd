(asdf:defsystem :arduino-serial
    :depends-on (sb-posix)
    :serial t
    :components
    ((:file "arduino-serial")))

