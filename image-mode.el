;;; image-mode.el --- support for visiting image files  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2005-2014 Free Software Foundation, Inc.
;;
;; Author: Richard Stallman <rms@gnu.org>
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
;; Defines a major mode for visiting image files that allows conversion between
;; viewing the text of the file and viewing the file as an image.  Viewing the
;; image works by putting a `display' text-property on the image data, with the
;; image-data still present underneath; if the resulting buffer file is saved to
;; another name it will correctly save the image data to the new file.
;;
;; Naming conventions:
;; 
;; Functions that operate on images and are meaningful outside of `image-mode'
;; don't have -mode- in their name. These functions must accept an optional
;; `image` argument which should default to `image-at-point'. The commands in
;; `image-manipulation-map' should not have -mode- in their name.
;;
;; Todo:
;;
;; Consolidate with doc-view to make them work on directories of images or on
;; image files containing various "pages".
;;
;;; Code:

(require 'image)
(require 'image-transform)

(defgroup image-mode ()
  "Support for visiting image files."
  :group 'multimedia)

(defcustom image-mode-auto-resize 'fit-if-large
  "Image auto-resize default.

If null, don't auto resize.  If set to a symbol, must take one of
the following values:

   *`fit' - maximally scale IMAGE to fit into window
   *`fit-if-large' - like `fit', but only when image is larger than window
   *`fit-height' - fit the image to window height
   *`fit-width' - fit the image to window width
   *`fit-stretch' - stretch the image to fit to both height and
    width of the window

A number is interpreted as width in pixels.  Cons cell, string and
list values are as the VALUE argument of `image-transform-spec:geometry'."
  :type '(choice
	  (const :tag "No Resize" nil)
	  (const :tag "Fit" fit)
	  (const :tag "Fit if Large" fit-if-large)
	  (const :tag "Fit Height" fit-height)
	  (const :tag "Fit Width" fit-width)
	  (const :tag "Fit Stretch" fit-stretch)
	  (number :tag "Width")
	  (string)
	  (cons number number)
	  (list number number string))
  :group 'image-mode
  :version "25.1")

(defcustom image-mode-show-cursor t
  "Non-nil if the cursor should be shown in `image-mode'."
  :group 'image-mode
  :type 'boolean)


;;; Window Info Management.

(defvar-local image-mode-winprops-alist t)

(defvar image-mode-new-window-functions nil
  "Special hook run when image data is requested in a new window.
It is called with one argument, the initial WINPROPS.")

(defun image-mode-winprops (&optional window cleanup)
  "Return winprops of WINDOW.
A winprops object has the shape (WINDOW . ALIST).  WINDOW defaults
to `selected-window' if it displays the current buffer, and
otherwise it defaults to t, used for times when the buffer is not
displayed.  When CLEANUP is non-nil, remove all winprops with
deleted window."
  (cond ((null window)
         (setq window
               (if (eq (current-buffer) (window-buffer)) (selected-window) t)))
        ((eq window t))
	((not (windowp window))
	 (error "Not a window: %s" window)))
  (when cleanup
    (setq image-mode-winprops-alist
  	  (delq nil (mapcar (lambda (winprop)
			      (let ((w (car-safe winprop)))
				(if (or (not (windowp w)) (window-live-p w))
				    winprop)))
  			    image-mode-winprops-alist))))
  (let ((winprops (assq window image-mode-winprops-alist)))
    ;; For new windows, set defaults from the latest.
    (if winprops
        ;; Move window to front.
        (setq image-mode-winprops-alist
              (cons winprops (delq winprops image-mode-winprops-alist)))
      (setq winprops (cons window
                           (copy-alist (cdar image-mode-winprops-alist))))
      ;; Add winprops before running the hook, to avoid inf-loops if the hook
      ;; triggers window-configuration-change-hook.
      (setq image-mode-winprops-alist
            (cons winprops image-mode-winprops-alist))
      (run-hook-with-args 'image-mode-new-window-functions winprops))
    winprops))

(defun image-mode-window-get (prop &optional winprops)
  (declare (gv-setter (lambda (val)
                        `(image-mode-window-put ,prop ,val ,winprops))))
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (cdr (assq prop (cdr winprops))))

(defun image-mode-window-put (prop val &optional winprops)
  (unless (consp winprops) (setq winprops (image-mode-winprops winprops)))
  (unless (eq t (car winprops))
    (image-mode-window-put prop val t))
  (setcdr winprops (cons (cons prop val)
                         (delq (assq prop (cdr winprops)) (cdr winprops)))))

(defun image-set-window-vscroll (vscroll)
  (setf (image-mode-window-get 'vscroll) vscroll)
  (set-window-vscroll (selected-window) vscroll))

(defun image-set-window-hscroll (ncol)
  (setf (image-mode-window-get 'hscroll) ncol)
  (set-window-hscroll (selected-window) ncol))

(defun image-mode-reapply-winprops ()
  ;; When set-window-buffer, set hscroll and vscroll to what they were
  ;; last time the image was displayed in this window.
  (when (listp image-mode-winprops-alist)
    ;; Beware: this call to image-mode-winprops can't be optimized away,
    ;; because it not only gets the winprops data but sets it up if needed
    ;; (e.g. it's used by doc-view to display the image in a new window).
    (let* ((winprops (image-mode-winprops nil t))
           (hscroll (image-mode-window-get 'hscroll winprops))
           (vscroll (image-mode-window-get 'vscroll winprops)))
      (when (image-at-point) ;Only do it if we display an image!
	(if hscroll (set-window-hscroll (selected-window) hscroll))
	(if vscroll (set-window-vscroll (selected-window) vscroll))))))

(defun image-mode-setup-winprops ()
  ;; Record current scroll settings.
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil))
  (add-hook 'window-configuration-change-hook
 	    'image-mode-reapply-winprops nil t))


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


;;; Image Scrolling

(defun image-mode-forward-hscroll (&optional n)
  "Scroll image in current window to the left by N character widths.
Stop if the right edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
	((< n 0)
	 (image-set-window-hscroll (max 0 (+ (window-hscroll) n))))
	(t
	 (let* ((image (image-at-point))
		(edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges)))
		(img-width (ceiling (car (image-display-size image)))))
	   (image-set-window-hscroll (min (max 0 (- img-width win-width))
					  (+ n (window-hscroll))))))))

(define-obsolete-function-alias 'image-forward-hscroll 'image-mode-forward-hscroll "25.1")

(defun image-mode-backward-hscroll (&optional n)
  "Scroll image in current window to the right by N character widths.
Stop if the left edge of the image is reached."
  (interactive "p")
  (image-mode-forward-hscroll (- n)))

(define-obsolete-function-alias 'image-backward-hscroll 'image-mode-backward-hscroll "25.1")

(defun image-mode-next-line (n)
  "Scroll image in current window upward by N lines.
Stop if the bottom edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
	((< n 0)
	 (image-set-window-vscroll (max 0 (+ (window-vscroll) n))))
	(t
	 (let* ((image (image-at-point))
		(edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges)))
		(img-height (ceiling (cdr (image-display-size image)))))
	   (image-set-window-vscroll (min (max 0 (- img-height win-height))
					  (+ n (window-vscroll))))))))

(define-obsolete-function-alias 'image-next-line 'image-mode-next-line)

(defun image-mode-previous-line (&optional n)
  "Scroll image in current window downward by N lines.
Stop if the top edge of the image is reached."
  (interactive "p")
  (image-mode-next-line (- n)))

(define-obsolete-function-alias 'image-previous-line 'image-mode-previous-line)

(defun image-mode-scroll-up (&optional n)
  "Scroll image in current window upward by N lines.
Stop if the bottom edge of the image is reached.
If ARG is omitted or nil, scroll upward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll downward.
If ARG is the atom `-', scroll downward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-mode-next-line
	    (max 0 (- win-height next-screen-context-lines)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-mode-next-line
	    (min 0 (- next-screen-context-lines win-height)))))
	(t (image-mode-next-line (prefix-numeric-value n)))))

(define-obsolete-function-alias 'image-scroll-up 'image-mode-scroll-up "25.1")

(defun image-mode-scroll-down (&optional n)
  "Scroll image in current window downward by N lines.
Stop if the top edge of the image is reached.
If ARG is omitted or nil, scroll downward by a near full screen.
A near full screen is `next-screen-context-lines' less than a full screen.
Negative ARG means scroll upward.
If ARG is the atom `-', scroll upward by nearly full screen.
When calling from a program, supply as argument a number, nil, or `-'."
  (interactive "P")
  (cond ((null n)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-mode-next-line
	    (min 0 (- next-screen-context-lines win-height)))))
	((eq n '-)
	 (let* ((edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges))))
	   (image-mode-next-line
	    (max 0 (- win-height next-screen-context-lines)))))
	(t (image-mode-next-line (- (prefix-numeric-value n))))))

(define-obsolete-function-alias 'image-scroll-down 'image-mode-scroll-down "25.1")

(defun image-mode-bol (arg)
  "Scroll horizontally to the left edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-mode-next-line (- arg 1)))
  (image-set-window-hscroll 0))

(define-obsolete-function-alias 'image-bol 'image-mode-bol)

(defun image-mode-eol (arg)
  "Scroll horizontally to the right edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-mode-next-line (- arg 1)))
  (let* ((image (image-at-point))
	 (edges (window-inside-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (img-width (ceiling (car (image-display-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))))

(define-obsolete-function-alias 'image-eol 'image-mode-eol)

(defun image-mode-bob ()
  "Scroll to the top-left corner of the image in the current window."
  (interactive)
  (image-set-window-hscroll 0)
  (image-set-window-vscroll 0))

(define-obsolete-function-alias 'image-bob 'image-mode-bob)

(defun image-mode-eob ()
  "Scroll to the bottom-right corner of the image in the current window."
  (interactive)
  (let* ((image (image-at-point))
	 (edges (window-inside-edges))
	 (win-width (- (nth 2 edges) (nth 0 edges)))
	 (img-width (ceiling (car (image-display-size image))))
	 (win-height (- (nth 3 edges) (nth 1 edges)))
	 (img-height (ceiling (cdr (image-display-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))
    (image-set-window-vscroll (max 0 (- img-height win-height)))))

(define-obsolete-function-alias 'image-eob 'image-mode-eob)


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


;;; Image Mode

(defvar-local image-type nil
  "The image type for the current Image mode buffer.")

(defvar image-mode-previous-major-mode nil
  "Internal variable to keep the previous non-image major mode.")

(defvar image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "\C-c\C-c" 'image-mode-toggle-display)
    (define-key map (kbd "SPC")       'image-mode-scroll-up)
    (define-key map (kbd "S-SPC")     'image-mode-scroll-down)
    (define-key map (kbd "DEL")       'image-mode-scroll-down)
    (define-key map (kbd "RET")       'image-toggle-animation)
    (define-key map "n" 'image-mode-next-file)
    (define-key map "p" 'image-mode-previous-file)
    (define-key map [remap forward-char] 'image-mode-forward-hscroll)
    (define-key map [remap backward-char] 'image-mode-backward-hscroll)
    (define-key map [remap right-char] 'image-mode-forward-hscroll)
    (define-key map [remap left-char] 'image-mode-backward-hscroll)
    (define-key map [remap previous-line] 'image-mode-previous-line)
    (define-key map [remap next-line] 'image-mode-next-line)
    (define-key map [remap scroll-up] 'image-mode-scroll-up)
    (define-key map [remap scroll-down] 'image-mode-scroll-down)
    (define-key map [remap scroll-up-command] 'image-mode-scroll-up)
    (define-key map [remap scroll-down-command] 'image-mode-scroll-down)
    (define-key map [remap move-beginning-of-line] 'image-mode-bol)
    (define-key map [remap move-end-of-line] 'image-mode-eol)
    (define-key map [remap beginning-of-buffer] 'image-mode-bob)
    (define-key map [remap end-of-buffer] 'image-mode-eob)
    (easy-menu-define image-mode-menu map "Menu for Image mode."
      '("Image"
	["Show as Text" image-mode-toggle-display :active t
	 :help "Show image as text"]
	"--"
	["Show Thumbnails" image-mode-show-thumbnails :active default-directory
	 :help "Show thumbnails for all images in this directory"]
	["Next Image" image-mode-next-file :active buffer-file-name
         :help "Move to next image in this directory"]
	["Previous Image" image-mode-previous-file :active buffer-file-name
         :help "Move to previous image in this directory"]
	))
    map)
  "Mode keymap for `image-mode'.")

(defvar image-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-mode-toggle-display)
    map)
  "Mode keymap for `image-minor-mode'.")

(defvar bookmark-make-record-function)

(put 'image-mode 'mode-class 'special)

;;;###autoload
(defun image-mode ()
  "Major mode for image files.
You can use \\<image-mode-map>\\[image-mode-toggle-display]
to toggle between display as an image and display as text.

Key bindings:
\\{image-mode-map}"
  (interactive)
  (condition-case err
      (progn
	(unless (display-images-p)
	  (error "Display does not support images"))

	(kill-all-local-variables)
	(setq major-mode 'image-mode)

	(if (not (image-at-point))
	    (progn
	      (image-mode-toggle-display-image)
	      ;; If attempt to display the image fails.
	      (if (not (image-at-point))
		  (error "Invalid image")))
	  ;; Set next vars when image is already displayed but local
	  ;; variables were cleared by kill-all-local-variables
	  (setq cursor-type nil truncate-lines t
		image-type (image-get (image-at-point) :type)))

	(setq mode-name (if image-type (format "Image[%s]" image-type) "Image"))
	(use-local-map image-mode-map)

	;; Use our own bookmarking function for images.
	(setq-local bookmark-make-record-function
		    #'image-bookmark-make-record)

	;; Keep track of [vh]scroll when switching buffers
	(image-mode-setup-winprops)

	(setq-local revert-buffer-function 'image-mode-revert-buffer)

	(add-hook 'change-major-mode-hook 'image-mode-toggle-display-text nil t)
	(add-hook 'after-revert-hook 'image-mode-after-revert nil t)
	(run-mode-hooks 'image-mode-hook)
	(let ((image (image-at-point))
	      (msg1 (substitute-command-keys
		     "Type \\[image-mode-toggle-display] to view the image as "))
	      animated)
	  (cond
	   ((null image)
	    (message "%s" (concat msg1 "an image.")))
	   ((setq animated (image-multi-frame-p image))
	    (setq mode-line-process
		  `(:eval
		    (concat " "
			    (propertize
			     (format "[%s/%s]"
				     (1+ (image-current-frame ',image))
				     ,(car animated))
			     'help-echo "Frames
mouse-1: Next frame
mouse-3: Previous frame"
			     'mouse-face 'mode-line-highlight
			     'local-map
			     '(keymap
			       (mode-line
				keymap
				(down-mouse-1 . image-next-frame)
				(down-mouse-3 . image-previous-frame)))))))
	    (message "%s"
		     (concat msg1 "text.  This image has multiple frames.")))
;;;			     (substitute-command-keys
;;;			      "\\[image-toggle-animation] to animate."))))
	   (t
	    (message "%s" (concat msg1 "text."))))))

    (error
     (image-mode-as-text)
     (funcall
      (if (called-interactively-p 'any) 'error 'message)
      "Cannot display image: %s" (cdr err)))))

(defun image-mode-revert-buffer (ignore _noconfirm)
  ;; fixme: revert without re-installing the mode; deriving modes is difficult
  (let ((revert-buffer-function nil))
    ;;Don't ask on reversion.
    (revert-buffer ignore t)))

;;;###autoload
(define-minor-mode image-minor-mode
  "Toggle Image minor mode in this buffer.
With a prefix argument ARG, enable Image minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Image minor mode provides the key \\<image-mode-map>\\[image-mode-toggle-display],
to switch back to `image-mode' and display an image file as the
actual image."
  nil (:eval (if image-type (format " Image[%s]" image-type) " Image"))
  image-minor-mode-map
  :group 'image
  :version "22.1"
  (if image-minor-mode
      (add-hook 'change-major-mode-hook (lambda () (image-minor-mode -1)) nil t)))

;;;###autoload
(defun image-mode-as-text ()
  "Set a non-image mode as major mode in combination with image minor mode.
A non-image major mode found from `auto-mode-alist' or Fundamental mode
displays an image file as text.  `image-minor-mode' provides the key
\\<image-mode-map>\\[image-mode-toggle-display] to switch back to `image-mode'
to display an image file as the actual image.

You can use `image-mode-as-text' in `auto-mode-alist' when you want
to display an image file as text initially.

See commands `image-mode' and `image-minor-mode' for more information
on these modes."
  (interactive)
  ;; image-mode-as-text = normal-mode + image-minor-mode
  (let ((previous-image-type image-type)) ; preserve `image-type'
    (if image-mode-previous-major-mode
	;; Restore previous major mode that was already found by this
	;; function and cached in `image-mode-previous-major-mode'
	(funcall image-mode-previous-major-mode)
      (let ((auto-mode-alist
	     (delq nil (mapcar
			(lambda (elt)
			  (unless (memq (or (car-safe (cdr elt)) (cdr elt))
					'(image-mode image-mode-maybe image-mode-as-text))
			    elt))
			auto-mode-alist)))
	    (magic-fallback-mode-alist
	     (delq nil (mapcar
			(lambda (elt)
			  (unless (memq (or (car-safe (cdr elt)) (cdr elt))
					'(image-mode image-mode-maybe image-mode-as-text))
			    elt))
			magic-fallback-mode-alist))))
	(normal-mode)
	(setq-local image-mode-previous-major-mode major-mode)))
    ;; Restore `image-type' after `kill-all-local-variables' in `normal-mode'.
    (setq image-type previous-image-type)
    ;; Enable image minor mode with `C-c C-c'.
    (image-minor-mode 1)
    ;; Show the image file as text.
    (image-mode-toggle-display-text)
    (message "%s" (concat
		   (substitute-command-keys
		    "Type \\[image-mode-toggle-display] to view the image as ")
		   (if (image-at-point)
		       "text" "an image") "."))))

(define-obsolete-function-alias 'image-mode-maybe 'image-mode "23.2")

(defun image-mode-toggle-display-text ()
  "Show the image file as text.
Remove text properties that display the image."
  (let ((inhibit-read-only t)
	(buffer-undo-list t)
	(modified (buffer-modified-p)))
    (remove-list-of-text-properties (point-min) (point-max)
				    '(display read-nonsticky ;; intangible
					      read-only front-sticky))
    (set-buffer-modified-p modified)
    (if (called-interactively-p 'any)
	(message "Repeat this command to go back to displaying the image"))))

(define-obsolete-function-alias 'image-toggle-display-text 'image-mode-toggle-display-text "25.1")

(defun image-mode-show-thumbnails ()
  "Show thumbnails alongside dired buffer.
Based on `image-dired'"
  (interactive)
  (image-dired default-directory))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)
(declare-function image-flush "image.c" (spec &optional frame))

(defun image-mode-toggle-display-image ()
  "Show the image of the image file.
Turn the image data into a real image, but only if the whole file
was inserted."
  (unless (derived-mode-p 'image-mode)
    (error "The buffer is not in Image mode"))
  (let* ((filename (buffer-file-name))
	 (data-p (not (and filename
			   (file-readable-p filename)
			   (not (file-remote-p filename))
			   (not (buffer-modified-p))
			   (not (and (boundp 'archive-superior-buffer)
				     archive-superior-buffer))
			   (not (and (boundp 'tar-superior-buffer)
				     tar-superior-buffer)))))
	 (file-or-data (if data-p
			   (string-make-unibyte
			    (buffer-substring-no-properties (point-min) (point-max)))
			 filename))
	 (image (create-image file-or-data nil data-p))
	 (type (plist-get (cdr image) :type))
	 ;; (type (image-type file-or-data nil data-p))
	 (inhibit-read-only t)
	 (buffer-undo-list t)
	 (modified (buffer-modified-p))
	 props)

    ;; Discard any stale image data before looking it up again.
    (image-flush image)
    (image-transform-interactive image :resize image-mode-auto-resize)
    
    (setq props
	  `(display ,image
		    ;; intangible ,image
		    rear-nonsticky (display) ;; intangible
		    read-only t front-sticky (read-only)
		    keymap ,image-manipulation-map))

    (let ((buffer-file-truename nil)) ; avoid changing dir mtime by lock_file
      (add-text-properties (point-min) (point-max) props)
      (restore-buffer-modified-p modified))

    ;; It's important to distinguish active buffer/image.
    (unless image-mode-show-cursor
      (setq cursor-type nil))

    ;; This just makes the arrow displayed in the right fringe
    ;; area look correct when the image is wider than the window.
    (setq truncate-lines t)
    ;; Disable adding a newline at the end of the image file when it
    ;; is written with, e.g., C-x C-w.
    (if (coding-system-equal (coding-system-base buffer-file-coding-system)
			     'no-conversion)
	(setq-local find-file-literally t))
    ;; Allow navigation of large images.
    (setq-local auto-hscroll-mode nil)
    (setq image-type type)
    (if (eq major-mode 'image-mode)
	(setq mode-name (format "Image[%s]" type)))
    (if (called-interactively-p 'any)
	(message "Repeat this command to go back to displaying the file as text"))))

(define-obsolete-function-alias 'image-toggle-display-image  'image-mode-toggle-display-image "25.1")

(defun image-mode-toggle-display ()
  "Toggle between image and text display.
If the current buffer is displaying an image file as an image,
call `image-mode-as-text' to switch to text.  Otherwise, display
the image by calling `image-mode'."
  (interactive)
  (if (image-at-point)
      (image-mode-as-text)
    (image-mode)))

(define-obsolete-function-alias 'image-toggle-display 'image-mode-toggle-display "25.1")

(defun image-mode-after-revert ()
  (when (image-at-point)
    (image-mode-toggle-display-text)
    ;; Update image display.
    (mapc (lambda (window) (redraw-frame (window-frame window)))
	  (get-buffer-window-list (current-buffer) 'nomini 'visible))
    (image-mode-toggle-display-image)))

(define-obsolete-function-alias 'image-after-revert-hook 'image-mode-after-revert "25.1")


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


;;; Switching to the next/previous image

(defun image-mode-next-file (&optional n)
  "Visit the next image in the same directory as the current image file.
With optional argument N, visit the Nth image file after the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p")
  (unless (derived-mode-p 'image-mode)
    (error "The buffer is not in Image mode"))
  (unless buffer-file-name
    (error "The current image is not associated with a file"))
  (let* ((file (file-name-nondirectory buffer-file-name))
	 (images (image-mode--images-in-directory file))
	 (idx 0))
    (catch 'image-visit-next-file
      (dolist (f images)
	(if (string= f file)
	    (throw 'image-visit-next-file (1+ idx)))
	(setq idx (1+ idx))))
    (setq idx (mod (+ idx (or n 1)) (length images)))
    (find-alternate-file (nth idx images))))

(define-obsolete-function-alias 'image-next-file 'image-mode-next-file "25.1")

(defun image-mode-previous-file (&optional n)
  "Visit the preceding image in the same directory as the current file.
With optional argument N, visit the Nth image file preceding the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p")
  (image-mode-next-file (- n)))

(defun image-mode--images-in-directory (file)
  (let* ((dir (file-name-directory buffer-file-name))
	 (files (directory-files dir nil
				 (image-file-name-regexp) t)))
    ;; Add the current file to the list of images if necessary, in
    ;; case it does not match `image-file-name-regexp'.
    (unless (member file files)
      (push file files))
    (sort files 'string-lessp)))

(define-obsolete-function-alias 'image-previous-file 'image-mode-previous-file "25.1")


;;; Support for bookmark.el
(declare-function bookmark-make-record-default
                  "bookmark" (&optional no-file no-context posn))
(declare-function bookmark-prop-get "bookmark" (bookmark prop))
(declare-function bookmark-default-handler "bookmark" (bmk))

(defun image-bookmark-make-record ()
  `(,@(bookmark-make-record-default nil 'no-context 0)
      (image-type . ,image-type)
      (handler    . image-bookmark-jump)))

;;;###autoload
(defun image-bookmark-jump (bmk)
  ;; This implements the `handler' function interface for record type
  ;; returned by `bookmark-make-record-function', which see.
  (prog1 (bookmark-default-handler bmk)
    (when (not (string= image-type (bookmark-prop-get bmk 'image-type)))
      (image-mode-toggle-display))))


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
    (define-key map "F" 'image-goto-frame)
    (define-key map "f" 'image-next-frame)
    (define-key map "b" 'image-previous-frame)
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
    (define-key map "ta" 'image-add-transform)
    (define-key map "td" 'image-delete-transform)
    (define-key map "tk" 'image-delete-transform)
    (define-key map "tm" 'image-modify-transform)
    (define-key map "tl" 'image-list-transforms)
    (define-key map "B" 'image-change-background)
    (easy-menu-define image-mode-menu map "Local Image Menu."
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


(provide 'image-mode)

;;; image-mode.el ends here
