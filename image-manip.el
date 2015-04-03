;;; image-manip.el --- Various utilities for image manipulation.  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2015 Free Software Foundation, Inc.
;;
;; Author: Vitalie Spinu <spinuvit@gmail.com>
;; Keywords: multimedia
;; Package: emacs
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A lot of these functions were taken with minor modifications from the old
;; image-mode.el.
;; 
;; fixme: remove all remaining references to image-mode
;; 
;;; Code:

(require 'image)
(require 'image-transform)



;;; Image Accessors

(defun image-get-display-property (&optional pos)
  (setq pos (or pos (point)))
  (or
   ;; display property
   (get-char-property pos 'display
		      ;; There might be different images for different displays.
		      (if (eq (window-buffer) (current-buffer))
			  (selected-window)))
   ;; overlay before-string/after-string display property, like in put-image
   (let ((OVS (overlays-at pos))
	 ov disp)
     (while (setq ov (pop OVS))
       (let ((bs (overlay-get ov 'before-string))
	     (as (overlay-get ov 'after-string)))
	 ;; last one takes precedence
	 (setq disp (or (and as (get-text-property 0 'display as))
			(and bs (get-text-property 0 'display bs))))))
     disp)
   ;; preceding point
   (and (not (bobp))
	(image-get-display-property (1- (point))))))

(defun image-at-point (&optional pos)
  "Return image at POS if there is one, or nil otherwise.
Search text and overlays at POS for a 'display' property that
holds an image.  POS defaults to point."
  (let* ((disp (image-get-display-property pos)))
    (or (and (eq (car-safe disp) 'image)
	     disp)
	;; margin images
	(and (eq (car-safe (cdr-safe disp)) 'image)
	     (cdr disp)))))

(defun image-get (image prop)
  "Extract a value from IMAGE corresponding to a given PROP."
  (declare (gv-setter (lambda (val)
                        `(image-put ,image ,prop ,val))))
  (unless (eq (car image) 'image)
    (error "Invalid image"))
  (plist-get (cdr image) prop))

(defun image-put (image prop val)
  "Change value in IMAGE of PROP to VAL.
The new image is returned. IMAGE is modified by side effects."
  (unless (eq (car image) 'image)
    (error "Invalid image"))
  (plist-put (cdr image) prop val)
  image)

(declare-function image-size "image.c" (spec &optional pixels frame))

(defun image-display-size (spec &optional pixels frame)
  "Wrapper around `image-size', handling slice display properties.
Like `image-size', the return value is (WIDTH . HEIGHT).
WIDTH and HEIGHT are in canonical character units if PIXELS is
nil, and in pixel units if PIXELS is non-nil.

If SPEC is an image display property, this function is equivalent
to `image-size'.  If SPEC is a list of properties containing
`image' and `slice' properties, return the display size taking
the slice property into account.  If the list contains `image'
but not `slice', return the `image-size' of the specified image."
  (if (eq (car spec) 'image)
      (image-size spec pixels frame)
    (let ((image (assoc 'image spec))
	  (slice (assoc 'slice spec)))
      (cond ((and image slice)
	     (if pixels
		 (cons (nth 3 slice) (nth 4 slice))
	       (cons (/ (float (nth 3 slice)) (frame-char-width frame))
		     (/ (float (nth 4 slice)) (frame-char-height frame)))))
	    (image
	     (image-size image pixels frame))
	    (t
	     (error "Invalid image specification: %s" spec))))))



;;; Adjust frame and image size.

(defun image-scale-frame-to-fit-image (&optional frame toggle image)
  "Fit FRAME to the image.
If FRAME is omitted or nil, it defaults to the selected frame.
All other windows on the frame are deleted.

If called interactively, or if TOGGLE is non-nil, toggle between
fitting FRAME to the IMAGE and restoring the size and window
configuration prior to the last `image-scale-frame-to-fit-image'
call.  IMAGE defaults to `image-at-point'."
  (interactive (list nil t))
  (let* ((buffer (current-buffer))
         (image (or image (image-at-point)))
         (size (image-display-size image))
	 (saved (frame-parameter frame 'image-mode-saved-params))
	 (window-configuration (current-window-configuration frame))
	 (width  (frame-width  frame))
	 (height (frame-height frame)))
    (with-selected-frame (or frame (selected-frame))
      (if (and toggle saved
	       (= (caar saved) width)
	       (= (cdar saved) height))
	  (progn
	    (set-frame-width  frame (car (nth 1 saved)))
	    (set-frame-height frame (cdr (nth 1 saved)))
	    (set-window-configuration (nth 2 saved))
	    (set-frame-parameter frame 'image-mode-saved-params nil))
	(delete-other-windows)
	(switch-to-buffer buffer t t)
	(let* ((edges (window-inside-edges))
	       (inner-width  (- (nth 2 edges) (nth 0 edges)))
	       (inner-height (- (nth 3 edges) (nth 1 edges))))
	  (set-frame-width  frame (+ (ceiling (car size))
				     width (- inner-width)))
	  (set-frame-height frame (+ (ceiling (cdr size))
				     height (- inner-height)))
	  ;; The frame size after the above `set-frame-*' calls may
	  ;; differ from what we specified, due to window manager
	  ;; interference.  We have to call `frame-width' and
	  ;; `frame-height' to get the actual results.
	  (set-frame-parameter frame 'image-mode-saved-params
			       (list (cons (frame-width)
					   (frame-height))
				     (cons width height)
				     window-configuration)))))))

(define-obsolete-function-alias 'image-mode-fit-frame 'image-scale-frame-to-fit-image "25.1")


;;; Animated images

(defcustom image-animate-loop nil
  "Non-nil means animated images loop forever, rather than playing once."
  :type 'boolean
  :version "24.1"
  :group 'image)

(defun image-toggle-animation (&optional image)
  "Start or stop animating IMAGE.
If `image-animate-loop' is non-nil, animation loops
forever.  Otherwise it plays once, then stops.  IMAGE defaults to
the image at point."
  (interactive)
  (let ((image (or image (image-at-point)))
	animation)
    (cond
     ((null image)
      (error "No image is present"))
     ((null (setq animation (image-multi-frame-p image)))
      (message "No image animation."))
     (t
      (let ((timer (image-animate-timer image)))
	(if timer
	    (cancel-timer timer)
	  (let ((index (plist-get (cdr image) :index)))
	    ;; If we're at the end, restart.
	    (and index
		 (>= index (1- (car animation)))
		 (setq index nil))
	    (image-animate image index
			   (if image-animate-loop t)))))))))

(defun image--set-speed (image speed &optional multiply)
  "Set speed of an animated IMAGE to SPEED.
If MULTIPLY is non-nil, treat SPEED as a multiplication factor.
If SPEED is `reset', reset the magnitude of the speed to 1.
IMAGE defaults to `image-at-point'."
  (let ((image (or image (image-at-point))))
    (cond
     ((null image)
      (error "No image is present"))
     ((null (image-multi-frame-p image))
      (message "No image animation."))
     (t
      (if (eq speed 'reset)
	  (setq speed (if (< (image-animate-get-speed image) 0)
			  -1 1)
		multiply nil))
      (image-animate-set-speed image speed multiply)
      ;; FIXME Hack to refresh an active image.
      (when (image-animate-timer image)
	(image-toggle-animation)
	(image-toggle-animation))
      (message "Image speed is now %s" (image-animate-get-speed image))))))

(defun image-increase-speed (&optional image)
  "Increase the speed of animated IMAGE by a factor of 2.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image--set-speed image 2 t))

(defun image-decrease-speed (&optional image)
  "Decrease the speed of animated IMAGE by a factor of 2.
IMAGE defaults to `image-at-point'"
  (interactive)
  (image--set-speed image 0.5 t))

(defun image-reverse-speed (&optional image)
  "Reverse the animation of the IMAGE.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image--set-speed image -1 t))

(defun image-reset-speed (&optional image)
  "Reset the animation speed of the IMAGE.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image--set-speed image 'reset))


;;; Multiframe Images

(defun image-goto-frame (n &optional relative image)
  "Show frame N of a multi-frame image.
Optional argument RELATIVE non-nil means interpret N as relative
to the current frame.  Frames are indexed from 1. IMAGE defaults
to `image-at-point'."
  (interactive
   (list (or current-prefix-arg
	     (read-number "Show frame number: "))))
  (let ((image (or image (image-at-point))))
    (cond
     ((null image)
      (error "No image is present"))
     ((null (image-multi-frame-p image))
      (message "No image animation."))
     (t
      (image-show-frame image
			(if relative
			    (+ n (image-current-frame image))
			  (1- n)))))))

(defun image-next-frame (&optional n image)
  "Switch to the next frame of a multi-frame image.
With optional argument N, switch to the Nth frame after the current one.
If N is negative, switch to the Nth frame before the current one.
IMAGE defaults to `image-at-point'."
  (interactive "p")
  (image-goto-frame n t image))

(defun image-previous-frame (&optional n image)
  "Switch to the previous frame of a multi-frame image.
With optional argument N, switch to the Nth frame before the
current one.  If N is negative, switch to the Nth frame after the
current one.  IMAGE defaults to `image-at-point'."
  (interactive "p")
  (image-next-frame (- n) image))


;;; Image Local Manipulation Map

(defcustom image-scale-step 1.1
  "Each step scales the image by this amount."
  :type 'number
  :group 'image)

(defun image-scale-adjust (&optional N)
  "Adjust the scale of the image by N steps.

N may be passed as a numeric prefix argument.  Each step scales
the image by the value of `image-scale-step' (a negative number
of steps decreases the height by the same amount).  As a special
case, an argument of 0 will remove any scaling currently active.

The actual adjustment depends on the final element of the
key binding used to invoke this command:

   +, =   Increase the size of the image by one step
   -      Decrease the size of the image by one step
   0      Reset to the original image size

When adjusting with `+' or `-', continue to read input events and
further adjust the scale as long as the input event read \(with
all modifiers removed) is `+' or `-'.

This command is a special-purpose wrapper around the
`image-scale-increase'."
  (interactive "p")
  (let ((ev last-command-event)
        (echo-keystrokes nil))
    (let* ((base (event-basic-type ev))
           (step
            (pcase base
              ((or ?+ ?=) N)
              (?- (- N))
              (?0 0)
              (t N))))
      (image-scale-increase step)
      (message "Use +,-,0 for further adjustment")
      (set-transient-map
       (let ((map (make-sparse-keymap)))
         (dolist (mods '(() (control)))
           (dolist (key '(?- ?+ ?= ?0)) ;; = is often unshifted +.
             (define-key map (vector (append mods (list key)))
               `(lambda () (interactive) (image-scale-adjust (floor (abs ,N)))))))
         map)))))

(defun image-scale-increase (&optional inc image)
  "Scale the the IMAGE by INC steps.
Each step scales up the size of the IMAGE the value of
`text-scale-mode-step' (a negative number of steps decreases the
size by the same amount).  As a special case, an argument of 0
will remove any scaling currently active. IMAGE defaults to
`image-at-point'."
  (interactive "p")
  (let  ((image (or image
		    (image-at-point)
		    (error "No image at point"))))
    (if (/= inc 0)
	(image-transform image :scale (* 100 (expt image-scale-step inc)))
      (image-tr--delete-properties image '(:width :height :resize))
      ;; don't touch :resize, It might have been set by initial 'fit-xxx operation
      (image-tr--delete-transforms image '(:scale))
      (image-transform image))
    (force-window-update (selected-window))))

(defun image-scale-decrease (&optional inc image)
  "Scale the IMAGE by INC steps.
Each step scales down the size of the IMAGE the value of
`text-scale-mode-step' (a negative number of steps increases the
size by the same amount).  As a special case, an argument of 0
will remove any scaling currently active. IMAGE defaults to
`image-at-point'."
  (interactive "p")
  (image-scale-increase (- inc) image))

(defun image-scale-to-fit-height (&optional image)
  "Fit IMAGE to the height of the current window.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image-transform-interactive image :resize 'fit-height))

(defun image-scale-to-fit-width (&optional image)
  "Fit IMAGE to the width of the current window.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image-transform-interactive image :resize 'fit-width))

(defun image-scale-to-fit-window (&optional image)
  "Maximally fit IMAGE into current window.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image-transform-interactive image :resize 'fit))

(defun image-stretch-to-fit-window (&optional image)
  "Stretch IMAGE into current window.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image-transform-interactive image :resize 'fit-stretch))

(defun image-rotate (rotation &optional image)
  "Prompt for an angle ROTATION, and rotate the IMAGE by that amount.
ROTATION should be in degrees.  IMAGE defaults to `image-at-point'."
  (interactive "nRotation angle (in degrees): ")
  (image-transform-interactive image :rotate rotation))

(defun image-rotate-right (&optional image)
  "Rotate the IMAGE clockwise by 90 degrees.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image-transform-interactive image :rotate 90))

(defun image-rotate-left (&optional image)
  "Rotate the IMAGE counter-clockwise by 90 degrees.
IMAGE defaults to `image-at-point'."
  (interactive)
  (image-transform-interactive image :rotate -90))

(defun image-change-background (&optional background image)
  "Set the BACKGROUND of the IMAGE.
For this to work, image must have a transparent background.  IMAGE
defaults to `image-at-point'."
  (interactive)
  (let ((bg (or background (read-color "Background: " t))))
    (unless image
      (unless (setq image (image-at-point))
        (error "No image at point")))
    (image-transform-interactive image :background bg)))

(defvar image-manipulation-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map "F" 'image-goto-frame)
    ;; (define-key map "f" 'image-next-frame)
    ;; (define-key map "b" 'image-previous-frame)
    (define-key map "a+" 'image-increase-speed)
    (define-key map "a-" 'image-decrease-speed)
    (define-key map "a0" 'image-reset-speed)
    (define-key map "ar" 'image-reverse-speed)
    (define-key map "+" 'image-scale-adjust)
    (define-key map "-" 'image-scale-adjust)
    (define-key map "=" 'image-scale-adjust)
    (define-key map "0" 'image-scale-adjust)
    (define-key map "r" 'image-rotate)
    (define-key map "]" 'image-rotate-right)
    (define-key map "[" 'image-rotate-left)
    (define-key map "ss" 'image-scale-to-fit-window)
    (define-key map "sh" 'image-scale-to-fit-height)
    (define-key map "sw" 'image-scale-to-fit-width)
    (define-key map "sS" 'image-stretch-to-fit-window)
    (define-key map "sf" 'image-scale-frame-to-fit-image)
    (define-key map "ta" 'image-transform-add)
    (define-key map "td" 'image-transform-delete)
    (define-key map "tk" 'image-transform-delete)
    (define-key map "tm" 'image-transform-modify)
    (define-key map "tl" 'image-transform-list)
    (define-key map "B" 'image-change-background)
    (easy-menu-define image-manipulation-menu map "Local Image Menu."
      '("Manipulate"
	("Transforms..."
	 :filter image-tr--all-transforms-menu)
	("Current Transforms..."
	 :active (cdr (image-get-transforms))
	 :filter image-tr--current-transforms-menu)
	"--"
	["Scale to Window" image-scale-to-fit-window
	 :help "Maximally resize image to fit into window"]
	["Scale to Window Height" image-scale-to-fit-height
	 :help "Resize image to match the window height"]
	["Scale to Window Width" image-scale-to-fit-width
	 :help "Resize image to match the window width"]
	["Scale Frame to Image" image-scale-frame-to-fit-image :active t
	 :help "Resize frame to match image"]
	["Scale Increase" (lambda () (interactive) (image-scale-increase 1))
	 :help "Scale Up" :keys "+/="]
	["Scale Decrease" (lambda () (interactive) (image-scale-increase -1))
	 :help "Scale Down" :keys "-"]
	["Scale Reset" (lambda () (interactive) (image-scale-increase 0))
	 :help "Scale Down" :keys "0"]
	"--"
	["Rotate Image by Angle" image-rotate]
	["Rotate Image Right" image-rotate-right]
	["Rotate Image Left" image-rotate-left]
	"--"
	("Animation..."
	 :active (image-multi-frame-p (image-at-point))
	 ["Animate Image" image-toggle-animation :style toggle
	  :selected (let ((image (image-at-point)))
		      (and image (image-animate-timer image)))
	  :help "Toggle image animation"]
	 ["Loop Animation"
	  (lambda () (interactive)
	    (setq image-animate-loop (not image-animate-loop))
	    ;; FIXME this is a hacky way to make it affect a currently
	    ;; animating image.
	    (when (let ((image (image-at-point)))
		    (and image (image-animate-timer image)))
	      (image-toggle-animation)
	      (image-toggle-animation)))
	  :style toggle :selected image-animate-loop
	  :help "Animate images once, or forever?"]
	 ["Reverse Animation" image-reverse-speed
	  :style toggle :selected (let ((image (image-at-point)))
				    (and image (<
						(image-animate-get-speed image)
						0)))
	  :help "Reverse direction of this image's animation?"]
	 ["Speed Up Animation" image-increase-speed
	  :help "Speed up this image's animation"]
	 ["Slow Down Animation" image-decrease-speed
	  :help "Slow down this image's animation"]
	 ["Reset Animation Speed" image-reset-speed
	  :help "Reset the speed of this image's animation"]
	 ["Next Frame" image-next-frame
	  :help "Show the next frame of this image"]
	 ["Previous Frame" image-previous-frame
	  :help "Show the previous frame of this image"]
	 ["Goto Frame..." image-goto-frame
	  :help "Show a specific frame of this image"])
	))
    map)
  "Image local manipulation keymap.
Usually used as keymap text property for images.

\\{image-manipulation-map}")

(provide 'image-manip)
