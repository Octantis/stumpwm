# LISP=sbcl
# clisp_BUILDOPTS=-K full -on-error exit < ./make-image.lisp
# sbcl_BUILDOPTS=--load ./make-image.lisp
# ccl_BUILDOPTS=--load ./make-image.lisp
# ecl_BUILDOPTS=-shell ./make-image.lisp
# CL_SOURCE_REGISTRY=$(PWD): $(LISP) $(@LISP@_BUILDOPTS)

all:
	CL_SOURCE_REGISTRY=$(PWD): sbcl --load ./make-image.lisp
