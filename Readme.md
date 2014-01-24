cl-v4l2 - minimal common lisp binding for video4linux 2
=======================================================


`cl-v4l2` is meant to be a portable common lisp binding for the v4l2
API.

I used the two different compilers Clozure Common Lisp and SBCL during
initial development. Clozure Common Lisp comes with a sophisticated
parser for C header files (ffigen4) which eases exploration of the
foreign library. I also implemented an interface using SBCL's own
sb-alien foreign function interface to understand .

The main code, however, is in `cffi/` and should be portable. It uses
the cffi groveller to obtain a few constants and datastructures from
<linux/videodev2.h>. I use `IOLib` for system calls (open, close,
ioctl and select).

## Example usage:

   (v4l-init :fn "/dev/video0" :w 640 :h 480)

   (loop for i below 100 collect
      (wait-and-read-frame))


## Author

* Martin Kielhorn (kielhorn.martin@gmail.com)

## Copyright

Copyright (c) 2013 Martin Kielhorn

## License

Licensed under the GPL License.