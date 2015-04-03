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
(require 'image-manip)
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

(defun image-mode-set-window-vscroll (vscroll)
  (setf (image-mode-window-get 'vscroll) vscroll)
  (set-window-vscroll (selected-window) vscroll))

(define-obsolete-function-alias 'image-set-window-vscroll 'image-mode-set-window-vscroll "25.1")

(defun image-mode-set-window-hscroll (ncol)
  (setf (image-mode-window-get 'hscroll) ncol)
  (set-window-hscroll (selected-window) ncol))

(define-obsolete-function-alias 'image-set-window-hscroll 'image-mode-set-window-hscroll "25.1")

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



;;; Image Scrolling

(defun image-mode-forward-hscroll (&optional n)
  "Scroll image in current window to the left by N character widths.
Stop if the right edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
	((< n 0)
	 (image-mode-set-window-hscroll (max 0 (+ (window-hscroll) n))))
	(t
	 (let* ((image (image-at-point))
		(edges (window-inside-edges))
		(win-width (- (nth 2 edges) (nth 0 edges)))
		(img-width (ceiling (car (image-display-size image)))))
	   (image-mode-set-window-hscroll (min (max 0 (- img-width win-width))
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
	 (image-mode-set-window-vscroll (max 0 (+ (window-vscroll) n))))
	(t
	 (let* ((image (image-at-point))
		(edges (window-inside-edges))
		(win-height (- (nth 3 edges) (nth 1 edges)))
		(img-height (ceiling (cdr (image-display-size image)))))
	   (image-mode-set-window-vscroll (min (max 0 (- img-height win-height))
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
  (image-mode-set-window-hscroll 0))

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
    (image-mode-set-window-hscroll (max 0 (- img-width win-width)))))

(define-obsolete-function-alias 'image-eol 'image-mode-eol)

(defun image-mode-bob ()
  "Scroll to the top-left corner of the image in the current window."
  (interactive)
  (image-mode-set-window-hscroll 0)
  (image-mode-set-window-vscroll 0))

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
    (image-mode-set-window-hscroll (max 0 (- img-width win-width)))
    (image-mode-set-window-vscroll (max 0 (- img-height win-height)))))

(define-obsolete-function-alias 'image-eob 'image-mode-eob)


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


(provide 'image-mode)

;;; image-mode.el ends here
