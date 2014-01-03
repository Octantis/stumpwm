;; Copyright (C) 2003-2008 Shawn Betts

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
;;
;; message printing functions
;;
;; Code:

(in-package #:stumpwm)

(export '(echo-string
          err
          message))

(defun max-width (font l)
  "Return the width of the longest string in L using FONT."
  (loop for i in l
        maximize (xlib:text-width font i :translate #'translate-id)))

(defun get-gravity-coords (gravity width height minx miny maxx maxy)
  "Return the x y coords for a window on with gravity etc"
  (values (case gravity
            ((:top-right :bottom-right :right) (- maxx width))
            ((:top :bottom :center) (truncate (- maxx minx width) 2))
            (t minx))
          (case gravity
            ((:bottom-left :bottom-right :bottom) (- maxy height))
            ((:left :right :center) (truncate (- maxy miny height) 2))
            (t miny))))

(defun setup-win-gravity (screen win gravity)
  "Position the x, y of the window according to its gravity. This
function expects to be wrapped in a with-state for win."
  (xlib:with-state ((screen-root screen))
    (let ((w (xlib:drawable-width win))
          (h (xlib:drawable-height win))
          (screen-width (head-width (current-head)))
          (screen-height (head-height (current-head))))
      (let ((x (case gravity
                 ((:top-left :bottom-left) 0)
                 (:center (truncate (- screen-width w (* (xlib:drawable-border-width win) 2)) 2))
                 (t (- screen-width w (* (xlib:drawable-border-width win) 2)))))
            (y (case gravity
                 ((:bottom-right :bottom-left) (- screen-height h (* (xlib:drawable-border-width win) 2)))
                 (:center (truncate (- screen-height h (* (xlib:drawable-border-width win) 2)) 2))
                 (t 0))))
        (setf (xlib:drawable-y win) (max (head-y (current-head)) (+ (head-y (current-head)) y))
              (xlib:drawable-x win) (max (head-x (current-head)) (+ (head-x (current-head)) x)))))))

(defun setup-message-window (screen lines width)
  (let ((height (* lines
                   (+ (xlib:font-ascent (screen-font screen))
                      (xlib:font-descent (screen-font screen)))))
        (win (screen-message-window screen)))
    ;; Now that we know the dimensions, raise and resize it.
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) height
            (xlib:drawable-width win) (+ width (* *message-window-padding* 2))
            (xlib:window-priority win) :above)
      (setup-win-gravity screen win *message-window-gravity*))
    (xlib:map-window win)
    (incf (screen-ignore-msg-expose screen))
    ;; Have to flush this or the window might get cleared
    ;; after we've already started drawing it.
    (xlib:display-finish-output *display*)))

(defun invert-rect (screen win x y width height)
  "invert the color in the rectangular area. Used for highlighting text."
  (let ((gcontext (xlib:create-gcontext :drawable win
                                        :foreground (get-color screen :fg)
                                        :function boole-xor)))
    (xlib:draw-rectangle win gcontext x y width height t)
    (setf (xlib:gcontext-foreground gcontext) (get-color screen :bg))
    (xlib:draw-rectangle win gcontext x y width height t)))

(defun unmap-message-window (screen)
  "Unmap the screen's message window, if it is mapped."
  (unless (eq (xlib:window-map-state (screen-message-window screen)) :unmapped)
    (xlib:unmap-window (screen-message-window screen))))

(defun unmap-all-message-windows ()
  (mapc #'unmap-message-window *screen-list*)
  (when (timer-p *message-window-timer*)
    (cancel-timer *message-window-timer*)
    (setf *message-window-timer* nil)))

(defun unmap-frame-indicator-window (screen)
  "Unmap the screen's message window, if it is mapped."
;;  (unless (eq (xlib:window-map-state (screen-frame-window screen)) :unmapped)
    (xlib:unmap-window (screen-frame-window screen)))

(defun unmap-all-frame-indicator-windows ()
  (mapc #'unmap-frame-indicator-window *screen-list*)
  (when (timer-p *frame-indicator-timer*)
    (cancel-timer *frame-indicator-timer*)
    (setf *frame-indicator-timer* nil)))

(defun reset-message-window-timer ()
  "Set the message window timer to timeout in *timeout-wait* seconds."
  (unless *ignore-echo-timeout*
    (when (timer-p *message-window-timer*)
      (cancel-timer *message-window-timer*))
    (setf *message-window-timer* (run-with-timer *timeout-wait* nil
                                                 'unmap-all-message-windows))))

(defun reset-frame-indicator-timer ()
  "Set the message window timer to timeout in *timeout-wait* seconds."
  (when (timer-p *frame-indicator-timer*)
    (cancel-timer *frame-indicator-timer*))
  (setf *frame-indicator-timer* (run-with-timer *timeout-frame-indicator-wait* nil
                                                'unmap-all-frame-indicator-windows)))

(defun show-frame-outline (group &optional (clear t))
  ;; Don't draw if this isn't a current group!
  (when (find group (mapcar 'screen-current-group *screen-list*))
    (dformat 5 "show-frame-outline!~%")
    ;; *resize-hides-windows* uses the frame outlines for display,
    ;; so try not to interfere.
    (unless (eq *top-map* *resize-map*)
      (when clear
        (clear-frame-outlines group))
      (let ((frame (tile-group-current-frame group)))
        (unless (and (= 1 (length (tile-group-frame-tree group)))
                     (atom (first (tile-group-frame-tree group))))
          ;; draw the outline
          (unless (frame-window frame)
            (draw-frame-outline group frame t t)))))))

(defun show-frame-indicator (group &optional force)
  (show-frame-outline group)
  ;; FIXME: Arg, these tests are already done in show-frame-outline
  (when (find group (mapcar 'screen-current-group *screen-list*))
    (when (or force
              (and (or (> (length (tile-group-frame-tree group)) 1)
                       (not (atom (first (tile-group-frame-tree group)))))
                   (not *suppress-frame-indicator*)))
      (let ((frame (tile-group-current-frame group))
            (w (screen-frame-window (current-screen)))
            (string (if (stringp *frame-indicator-text*)
                        *frame-indicator-text*
                        (prin1-to-string *frame-indicator-text*)))
            (font (screen-font (current-screen))))
        ;; If it's already mapped it'll appear briefly in the wrong
        ;; place, so unmap it first.
        (xlib:unmap-window w)
        (xlib:with-state (w)
          (setf (xlib:drawable-x w) (+ (frame-x frame)
                                       (truncate (- (frame-width frame) (xlib:text-width font string)) 2))
                (xlib:drawable-y w) (+ (frame-display-y group frame)
                                       (truncate (- (frame-height frame) (font-height font)) 2))
                (xlib:window-priority w) :above))
        (xlib:map-window w)
        (echo-in-window w font (get :bg-color (current-screen) :fg) (get-color (current-screen) :bg) string)
        (reset-frame-indicator-timer)))))

(defun echo-in-window (win font fg bg string)
  (let* ((height (font-height font))
         (gcontext (xlib:create-gcontext :drawable win
                                         :font font
                                         :foreground fg
                                         :background bg))
         (width (xlib:text-width font string)))
    (xlib:with-state (win)
      (setf (xlib:drawable-height win) height
            (xlib:drawable-width win) width))
    (xlib:clear-area win)
    (xlib:display-finish-output *display*)
    (xlib:draw-image-glyphs win gcontext 0 (xlib:font-ascent font) string :translate #'translate-id :size 16)))

(defun push-last-message (screen strings highlights)
  ;; only push unique messages
  (unless *record-last-msg-override*
    (push strings (screen-last-msg screen))
    (push highlights (screen-last-msg-highlights screen))
    ;; crop for size
    (when (>= (length (screen-last-msg screen)) *max-last-message-size*)
      (setf (screen-last-msg screen) (butlast (screen-last-msg screen)))
      (setf (screen-last-msg-highlights screen) (butlast (screen-last-msg-highlights screen))))))

(defun redraw-current-message (screen)
  (let ((*record-last-msg-override* t)
        (*ignore-echo-timeout* t))
    (dformat 5 "Redrawing message window!~%")
    (apply 'echo-string-list screen (screen-current-msg screen) (screen-current-msg-highlights screen))))

(defun echo-nth-last-message (screen n)
  (let ((*record-last-msg-override* t))
    (apply 'echo-string-list screen (nth n (screen-last-msg screen)) (nth n (screen-last-msg-highlights screen)))))

(defun echo-string-list (screen strings &rest highlights)
  "Draw each string in l in the screen's message window. HIGHLIGHT is
  the nth entry to highlight."
  (when strings
    (unless *executing-stumpwm-command*
      (let ((width (render-strings screen (screen-message-cc screen) *message-window-padding* 0 strings '() nil)))
        (setup-message-window screen (length strings) width)
        (render-strings screen (screen-message-cc screen) *message-window-padding* 0 strings highlights))
      (setf (screen-current-msg screen)
            strings
            (screen-current-msg-highlights screen)
            highlights)
      ;; Set a timer to hide the message after a number of seconds
      (if *suppress-echo-timeout*
          ;; any left over timers need to be canceled.
          (when (timer-p *message-window-timer*)
            (cancel-timer *message-window-timer*)
            (setf *message-window-timer* nil))
          (reset-message-window-timer)))
    (push-last-message screen strings highlights)
    (xlib:display-finish-output *display*)
    (dformat 5 "Outputting a message:~%~{        ~a~%~}" strings)
    (apply 'run-hook-with-args *message-hook* strings)))

(defun echo-string (screen msg)
  "Display @var{string} in the message bar on @var{screen}. You almost always want to use @command{message}."
  (echo-string-list screen (split-string msg (string #\Newline))))

(defun message (fmt &rest args)
  "run FMT and ARGS through `format' and echo the result to the current screen."
  (echo-string (current-screen) (apply 'format nil fmt args)))


(defun err (fmt &rest args)
  "run FMT and ARGS through format and echo the result to the
current screen along with a backtrace. For careful study, the
message does not time out."
  (let ((*suppress-echo-timeout* t))
    (echo-string (current-screen)
                 (concat (apply 'format nil fmt args)
                         (backtrace-string)))))

(defun message-no-timeout (fmt &rest args)
  "Like message, but the window doesn't disappear after a few seconds."
  (let ((*suppress-echo-timeout* t))
    (apply 'message fmt args)))

;;; Commands

(defvar *lastmsg-nth* nil)

(defcommand lastmsg () ()
  "Display the last message. If the previous command was lastmsg, then
continue cycling back through the message history."
  (if (string= *last-command* "lastmsg")
      (progn
        (incf *lastmsg-nth*)
        (if (>= *lastmsg-nth* (length (screen-last-msg (current-screen))))
            (setf *lastmsg-nth* 0)))
      (setf *lastmsg-nth* 0))
  (if (screen-last-msg (current-screen))
      (echo-nth-last-message (current-screen) *lastmsg-nth*)
      (message "No last message.")))

;; Message colors
;; This simplified implementation of the the C color code is as follows:
;;
;; ^B bright
;; ^b dim
;; ^n normal (sgr0)
;;
;; ^00 black black
;; ^10 red black
;; ^01 black red
;; ^1* red clear
;;
;; and so on.
;;
;; I won't explain here the many reasons that C is better than ANSI, so just
;; take my word for it.
; (defvar *colors*
;   '("black"
;     "red"
;     "green"
;     "yellow"
;     "blue"
;     "magenta"
;     "cyan"
;     "white")
;   "Eight colors by default. You can redefine these to whatever you like and
; then call (update-color-map).")
;
; (defun adjust-color (color amt)
;   (labels ((max-min (x y) (max 0 (min 1 (+ x y)))))
;     (setf (xlib:color-red color) (max-min (xlib:color-red color) amt)
;           (xlib:color-green color) (max-min (xlib:color-green color) amt)
;           (xlib:color-blue color) (max-min (xlib:color-blue color) amt))))
;   (xlib:alloc-color (xlib:screen-default-colormap (screen-number screen)) color))
; ;; Normal colors are dimmed and bright colors are intensified in order
; ;; to more closely resemble the VGA pallet.
; (defun update-color-map (screen)
;   "Read *colors* and cache their pixel colors for use when rendering colored text."
;   (let ((scm (xlib:screen-default-colormap (screen-number screen))))
;     (labels ((map-colors (amt)
;                (loop for c in *colors*
;                   as color = (handler-case (xlib:lookup-color scm c)
;                                (xlib:name-error (ne)
;                                  (hex c)))
;                   do (adjust-color color amt)
;                   collect (xlib:alloc-color scm color))))
;       (setf (screen-color-map-normal screen) (apply #'vector (map-colors 0.0))
;             (screen-color-map-bright screen) (apply #'vector (map-colors 0.25))))))
;
; (defun update-screen-color-context (screen)
;   (let* ((cc (screen-message-cc screen))
;          (bright (if (stringp *text-color*)
;                      (lookup-color screen *text-color*)
;                      *text-color*)))
;     (setf
;      (ccontext-default-fg cc) (screen-fg-color screen)
;      (ccontext-default-bg cc) (screen-bg-color screen))
;     (adjust-color bright 0.25)
;     (setf (ccontext-default-bright cc) (alloc-color screen bright))))
;
; (defun get-bg-color (screen cc color)
;   (setf *background* color)
;   (if color
;       (svref (screen-color-map-normal screen) color)
;       (ccontext-default-bg cc)))
;
; (defun get-fg-color (screen cc color)
;   (setf *foreground* color)
;   (if color
;       (svref *color-map* color)
;       (if (eq *color-map* (screen-color-map-bright screen))
;           (ccontext-default-bright cc)
;           (ccontext-default-fg cc))))
;
(defun cc-set-color (screen cc s i)
  (declare (ignore screen cc))
  (let* (;;(gc (ccontext-gc cc))
         (l (- (length s) i))
         (r 2)
         (f (subseq s i (1+ i)))
         (b (if (< l 2) "*" (subseq s (1+ i) (+ i 2)))))
    (labels
        ((set-fg-bg () ;;(fg bg)
           )

           ;; (if *reverse*
           ;;     (setf
           ;;      (xlib:gcontext-foreground gc) bg
           ;;      (xlib:gcontext-background gc) fg)
           ;;     (setf
           ;;      (xlib:gcontext-foreground gc) fg
           ;;      (xlib:gcontext-background gc) bg)))
         (update-colors ()
           ))
           ;; (set-fg-bg (get-fg-color screen cc *foreground*)
           ;;            (get-bg-color screen cc *background*))))
      (case (elt f 0)
        (#\n                            ; normal
         (setf f "*" b "*" r 1
               ;; *color-map* (screen-color-map-normal screen)
               ;; *reverse* nil)
               )
         ;; (get-fg-color screen cc nil)
         ;; (get-bg-color screen cc nil))
         )
        (#\b                            ; bright off
         ;; (setf *color-map* (screen-color-map-normal screen))
         (update-colors)
         (return-from cc-set-color 1))
        (#\B                            ; bright on
         ;; (setf *color-map* (screen-color-map-bright screen))
         (update-colors)
         (return-from cc-set-color 1))
        (#\R
         ;; (setf *reverse* t)
         (update-colors)
         (return-from cc-set-color 1))
        (#\r
         ;; (setf *reverse* nil)
         (update-colors)
         (return-from cc-set-color 1))
        (#\[
         ;; (push (list *foreground* *background* *color-map*) *color-stack*)
         (return-from cc-set-color 1))
        (#\]
         ;; (let ((colors (pop *color-stack*)))
         ;;   (when colors
         ;;     (setf *foreground* (first colors)
         ;;           *background* (second colors)
         ;;           *color-map* (third colors))))
         (update-colors)
         (return-from cc-set-color 1))
        (#\^                            ; circumflex
         (return-from cc-set-color 1)))
      ;; (handler-case
      ;;     (let ((fg (if (equal f "*") (progn (get-fg-color screen cc nil) (ccontext-default-fg cc)) (get-fg-color screen cc (parse-integer f))))
      ;;           (bg (if (equal b "*") (progn (get-bg-color screen cc nil) (ccontext-default-bg cc)) (get-bg-color screen cc (parse-integer b)))))
      ;;       (set-fg-bg fg bg))
      ;;   (error (c) (dformat 1 "Invalid color code: ~A" c))))
      )
    r))

(defun render-strings (screen cc padx pady strings highlights &optional (draw t))
  (let* ((height (+ (xlib:font-descent (screen-font screen))
                    (xlib:font-ascent (screen-font screen))))
         (width 0)
         (gc (ccontext-gc cc))
         (win (ccontext-win cc))
         (px (ccontext-px cc)))
    (when draw
      (when (or (not px)
                (/= (xlib:drawable-width px) (xlib:drawable-width win))
                (/= (xlib:drawable-height px) (xlib:drawable-height win)))
        (when px (xlib:free-pixmap px))
        (setf px (xlib:create-pixmap :drawable win
                                     :width (xlib:drawable-width win)
                                     :height (xlib:drawable-height win)
                                     :depth (xlib:drawable-depth win))
              (ccontext-px cc) px))
      (xlib:with-gcontext (gc :foreground (xlib:gcontext-background gc))
        (xlib:draw-rectangle px gc 0 0 (xlib:drawable-width px) (xlib:drawable-height px) t)))
    (loop for s in strings
          ;; We need this so we can track the row for each element
          for i from 0 to (length strings)
          do (let ((x 0) (off 0) (len (length s)))
               (loop
                for st = 0 then (+ en (1+ off))
                as en = (position #\^ s :start st)
                do (progn
		     (let ((en (cond ((and en (= (1+ en) len)) nil)
				     ((and en (char= #\^ (char s (1+ en)))) (1+ en))
				     (t en))))
		       (when draw
                         (xlib:draw-image-glyphs px gc
                                                 (+ padx x)
                                                 (+ pady (* i height)
                                                    (xlib:font-ascent (screen-font screen)))
                                                 (subseq s st en)
                                                 :translate #'translate-id
                                                 :size 16))
		       (setf x (+ x (xlib:text-width (screen-font screen) (subseq s st en) :translate #'translate-id))
			     width (max width x)))
		     (when (and en (< (1+ en) len))
		       ;; right-align rest of string?
		       (if (char= #\> (char s (1+ en)))
			   (progn
			     (when draw
			       (setf x (- (xlib:drawable-width px) (* 2 padx)
					  ;; get width of rest of s
					  (render-strings screen cc padx pady
							  (list (subseq s (+ en 2)))
							  '() nil))
				     width (- (xlib:drawable-width px) (* 2 padx))))
			     (setf off 1))
			   (setf off (cc-set-color screen cc s (1+ en))))))
		  while en))
          when (find i highlights :test 'eql)
          do (when draw (invert-rect screen px
                                     0 (* i height)
                                     (xlib:drawable-width px)
                                     height)))
    (when draw
      (xlib:copy-area px gc 0 0 (xlib:drawable-width px) (xlib:drawable-height px) win 0 0))
    (cc-set-color screen cc "n" 0)
    width))

; ;;; FIXME: It would be nice if the output of this parser was used to
; ;;; draw the text, but the current drawing implementation is probably
; ;;; faster.
(defun parse-color (s i)
  (let ((l (- (length s) i)))
    (when (zerop l)
      (return-from parse-color (values `("^") 0)))
    (let ((f (subseq s i (1+ i)))
          (b (if (< l 2) "*" (subseq s (1+ i) (+ i 2)))))
      (case (elt f 0)
        (#\n                            ; normal
         (values
          `((:background "*")
            (:foreground "*")
            (:reverse nil))
          1))
        (#\b                            ; bright off
         (values
          `((:bright nil))
          1))
        (#\B                            ; bright on
         (values
          `((:bright t))
          1))
        (#\R
         (values
          `((:reverse t))
          1))
        (#\r
         (values
          `((:reverse nil))
          1))
        (#\[
         (values
          `((:push))
          1))
        (#\]
         (values
          `((:pop))
          1))
        (#\^                            ; circumflex
         (values `("^") 1))
        ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
         (values
          `((:background ,(if (string= f "*")
                              "*"
                              (parse-integer f)))
            (:foreground ,(if (string= b "*")
                              "*"
                              (parse-integer b))))
          2))
        (t
         (values `(,(format nil "^~a" f)) 1))))))

(defun parse-color-string (string)
  "parse a color coded string into a list of strings and color codes"
  (loop
     with color = nil
     with off = 0
     for st = 0 then (min (+ en (1+ off)) (length string))
     as en = (position #\^ string :start st)
     ;; avoid empty strings at the beginning and end
     unless (or (eql en st)
                (eql st (length string)))
     collect (subseq string st en)
     while en
     append (progn
              (multiple-value-setq (color off) (parse-color string (1+ en)))
              color)))

(defun uncolorify (string)
  "Remove any color markup in STRING"
  (format nil "~{~a~}" (remove-if-not 'stringp (parse-color-string string))))
