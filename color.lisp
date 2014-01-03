;; Copyright (C) 2007-2008 Jonathan Moore Liles
;;
;;  This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, see
;; <http://www.gnu.org/licenses/>.

;; Commentary:
;; Functions to declare colors for use in themes.
;;

(in-package :stumpwm)

;; for use in stupmwm-user
(export '(set-color get-color))

(defvar *colors* '())

(defun lookup-color (screen color)
  "Takes the screen object, runs the xlib"
  (xlib:lookup-color (xlib:screen-default-colormap (screen-number screen)) color))

(defun hex (hex)
  "Converts a hexadecimal representation of a color to a decimal from [0,1)."
  (labels ((convert (x)
             (/ (read-from-string (concat "#x" x)) 256.0)))
    (assert (and (eql (elt hex 0) #\#) (= (length hex) 7)))
    (let ((red (subseq hex 1 3))
          (green (subseq hex 3 5))
          (blue (subseq hex 5 7)))
      (xlib:make-color :red (funcall #'convert red)
                       :green (funcall #'convert green)
                       :blue (funcall #'convert blue)))))

(defun alloc-color (snum input)
  "Given screen-number property of a screen object
and input that is a hex string or a plist with the values being hex strings
Wrapper for xlib:alloc-color"
  (flet ((ac (c)
           (xlib:alloc-color (xlib:screen-default-colormap snum) c)))
    (cond
      ((listp input)
       (mapcar (lambda (i) (if (and (stringp i)
                                    (eq (elt i 0) #\#))
                               (ac (hex i))
                               i))
               input))
      ((stringp input)
       (ac (hex input))))))

(defun set-colour-init (id-screen number-screen val)
  "Set the colour in the plist and update the screen's colors"
  (setf (getf *colors* id-screen)
        (alloc-color number-screen val)))

(defun set-color (name val &optional init)
  "Set the colour in the plist and update the screen's colors"
  (dolist (s *screen-list*)
    (setf (getf (getf *colors* (screen-id s)) name)
          (alloc-color (screen-number s) val)))
  (unless init
    (update-colors-all-screens)
    (if (current-window) (update-decoration (current-window)))))

(defun get-color (screen name &key version)
  "Return the xlib color. use 'version' to help decide which color for the name"
  (let (s-id name-val)
    (setf s-id (getf *colors* (screen-id screen)))
                                        ; (unless s-id (error "No plist data for that id"))
    (setf name-val (getf s-id name))
    (if (listp name-val)
        (let ((val (getf name-val version)))
          (if val
              val
              (getf name-val :t)))
        name-val)))
