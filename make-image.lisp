;; (require 'asdf)
;; (asdf:load-system :stumpwm)

;; #+sbcl

(require :stumpwm)

(sb-ext:save-lisp-and-die
 "stumpwm"
 :toplevel
 (lambda ()
   ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
   (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
   (stumpwm:stumpwm)
   0)
 :executable t)
