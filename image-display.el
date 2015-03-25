;;; image-display.el --- support for displaying multiple image files  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2014-2015 Free Software Foundation, Inc.
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
;;; Code:

(require 'image)
(require 'image-transform)
(require 'ring)

(defgroup image-display ()
  "Support for displaying multiple image files."
  :group 'multimedia)

(defcustom image-display-auto-resize 'fit
  "Image auto-resize default.

If null, don't auto resize.  If set to a symbol, must take one of
the following values:

   *`fit' - maximally scale IMAGE to fit into allowed box or window
   *`fit-if-large' - like `fit' but only when larger than the box/window
   *`fit-height' - fit the image to box/window height
   *`fit-width' - fit the image to box/window width
   *`fit-stretch' - stretch the image to fit height and width of
    the box/window

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
  :group 'image-display
  :version "25.1")

(defcustom image-display-default-page-geom 3
  "Default geometry of the image-display page.
Can be a number or a cons of the form (rows . cols). A number
represents a total number of images per page. In this case the
number of rows and cols are computed sensibly based on the width
and height of the current window."
  :group 'image-display
  :type '(choice integer (cons integer integer)))

(defvar-local image-display-ring nil
  "Ring of images in the current buffer.")
(put 'image-display-ring 'permanent-local t)

(defvar-local image-display-ring-index nil
  "Ring of images in the current buffer.")
(put 'image-display-ring-index 'permanent-local t)


;;; Pages
(defvar image-display-page-start-delimiter "^ISTART"
  "Sequence of characters that start a multi image portion of the buffer.")
(defvar image-display-page-end-delimiter "^IEND"
  "Sequence of characters that end a multi image portion of the buffer.")
(defvar-local image-display-page-size nil)

(defun image-display-page-start (&optional pos)
  "Return the page start position preceding POS or point-min if not found."
  (save-excursion
    (goto-char (or pos (point-max)))
    (or (and (re-search-backward image-display-page-start-delimiter nil t)
	     (match-end 0))
	(point-min))))

(defun image-display-page-end (&optional pos)
  "Return end of image page following POS or point-max if not found."
  (save-excursion
    (goto-char (or pos (point)))
    (or (and (re-search-forward image-display-page-start-delimiter nil t)
	     (match-beginning 0))
	(point-max))))

(defun image-display--compute-page-size (geom)
  "Return a list of the form (R C W H) from GEOM.
R and C are the number of rows and columns. W and H are the width
and height in pixels of the box to fit each image in. GEOM is as
in `image-display-default-page-geom'."
  (let* ((wedges (window-inside-pixel-edges))
	 (wh (- (nth 3 wedges) (nth 1 wedges) (frame-char-height)))
	 (ww (- (nth 2 wedges) (nth 0 wedges) (frame-char-width)))
	 (geom (cond ((consp geom)
		      (unless (and (numberp (car geom)) (numberp (cdr geom)))
			(error "Rows and columns in page size specification must be numbers"))
		      (when (or (< (car geom) 0) (< (cdr geom) 1))
			(error "Rows and columns in page geom specification must be positive."))
		      geom)
		     ((numberp geom)
		      ;; compute cols and rows such that each image box is
		      ;; approximately square
		      (let ((cols (round (sqrt (/ (* ww geom) (float wh))))))
			(cons (ceiling (/ (float geom) cols))
			      cols))))))
    (let* ((w (- (/ ww (car geom)) (frame-char-width)))
	   (h (- (/ wh (cdr geom)) (frame-char-height)))
	   (N (* (car geom) (cdr geom))))
      (list (car geom) (cdr geom) w h))))
;; (image-display--compute-page-size 6)

(defcustom image-display-cursor-color "gray30"
  "Color of the current image border and background `image-display-mode'."
  :group 'image-display)

(defcustom image-display-border-width (/ (frame-char-height) 2)
  "Border around images in `image-display-mode' buffers."
  :group 'image-display)

(defun image-display--get-image-span (&optional pos)
  (let ((inhibit-point-motion-hooks t)
	(pos (or pos (point))))
    (cons pos (next-single-property-change pos 'field nil (point-max)))))

(defun image-display--point-entered-handler (old new)
  (let ((inhibit-point-motion-hooks t)
	(span (image-display--get-image-span new)))
    (move-overlay image-display--cursor-overlay (car span) (cdr span))))

(defun image-display-insert-page (&optional pos)
  "Display images from image ring associated with the page at POS."
  (let* ((inhibit-read-only t)
	 (buffer-undo-list t)
	 (page-start (image-display-page-start pos))
	 (page-end (image-display-page-end page-start))
	 (ring (or (get-text-property page-start :image-display-ring)
		   image-display-ring
		   (error "No image ring found")))
	 (index (or (get-text-property page-start :image-display-ring-index)
		    image-display-ring-index
		    0))
	 (psize (or (get-text-property page-start :image-display-page-size)
		    image-display-page-size
		    image-display-default-page-geom))
	 (geom (image-display--compute-page-size psize))
	 (N (* (car geom) (cadr geom))))

    ;; (let ((buffer-file-truename nil)) ; avoid changing dir mtime by lock_file
    ;;   (add-text-properties (point-min) (point-max) props)
    ;;   (restore-buffer-modified-p modified))
    ;; ;; Discard any stale image data before looking it up again.
    ;; (image-flush image)
    ;; (image-transform-interactive image :resize image-display-auto-resize)
    ;; Disable adding a newline at the end of the image file when it
    ;; is written with, e.g., C-x C-w.
    ;; (if (coding-system-equal (coding-system-base buffer-file-coding-system)
    ;; 			   'no-conversion)
    ;;     (setq-local find-file-literally t))
    (switch-to-buffer (current-buffer))
    (with-silent-modifications
      (goto-char page-start)
      (delete-region page-start page-end)
      (cl-loop for i from 1 to N
	       for ix = (+ index i -1)
	       while (< ix (ring-length ring)) do
	       (let* ((img (ring-ref ring ix))
		      (img (if (stringp img)
			       (create-image img)
			     img))
		      (img (image-transform img
					    :resize image-display-auto-resize
					    :box (cddr geom)))
		      (name (concat (or (image-get img :file)
					(number-to-string i))
				    " "))
		      (face `(:box (:line-width ,image-display-border-width
						:color ,(face-attribute 'default :background))))
		      (str (propertize name 'intangible i 'field i
				       'face face
				       'point-entered #'image-display--point-entered-handler)))
		 (insert-image img str nil nil t)
		 (unless nil ;; (= i N)
		   (if (zerop (mod i (cadr geom)))
		       (insert (propertize "\n" 'intangible i))
		     ;; emacs doesn't display horizontal border correctly, this is an awkward fix
		     (insert (propertize " " 'intangible i 'field i 'face face)))))))
    (goto-char page-start)))



;;; Display Mode

(defvar image-display-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; (define-key map "\C-c\C-c" 'image-display-toggle)
    (define-key map (kbd "SPC")       'image-mode-scroll-up)
    (define-key map (kbd "S-SPC")     'image-mode-scroll-down)
    (define-key map (kbd "DEL")       'image-mode-scroll-down)
    (define-key map (kbd "RET")       'image-toggle-animation)
    (define-key map "n" 'image-mode-next-file)
    (define-key map "p" 'image-mode-previous-file)
    ;; (define-key map [remap forward-char] 'image-mode-forward-hscroll)
    ;; (define-key map [remap backward-char] 'image-mode-backward-hscroll)
    ;; (define-key map [remap right-char] 'image-mode-forward-hscroll)
    ;; (define-key map [remap left-char] 'image-mode-backward-hscroll)
    ;; (define-key map [remap previous-line] 'image-mode-previous-line)
    ;; (define-key map [remap next-line] 'image-mode-next-line)
    ;; (define-key map [remap scroll-up] 'image-mode-scroll-up)
    ;; (define-key map [remap scroll-down] 'image-mode-scroll-down)
    ;; (define-key map [remap scroll-up-command] 'image-mode-scroll-up)
    ;; (define-key map [remap scroll-down-command] 'image-mode-scroll-down)
    ;; (define-key map [remap move-beginning-of-line] 'image-mode-bol)
    ;; (define-key map [remap move-end-of-line] 'image-mode-eol)
    ;; (define-key map [remap beginning-of-buffer] 'image-mode-bob)
    ;; (define-key map [remap end-of-buffer] 'image-mode-eob)
    (easy-menu-define image-mode-menu map "Menu for Image mode."
      '("Image"
	;; ["Show as Text" image-mode-toggle-display :active t
	;;  :help "Show image as text"]
	;; "--"
	["Show Thumbnails" image-mode-show-thumbnails :active default-directory
	 :help "Show thumbnails for all images in this directory"]
	["Next Image" image-mode-next-file :active buffer-file-name
         :help "Move to next image in this directory"]
	["Previous Image" image-mode-previous-file :active buffer-file-name
         :help "Move to previous image in this directory"]
	))
    map)
  "Mode keymap for `image-mode'.")

(defvar image-display-mode-line-process
  '(:eval
    (let* ((image (image-at-point))
	   (animated (image-multi-frame-p image)))
      (concat " "
	      (when animated
		(propertize
		 (format "[%s/%s]"
			 (1+ (image-current-frame image))
			 (car animated))
		 'help-echo "Frames\nmouse-1: Next frame\nmouse-3: Previous frame"
		 'mouse-face 'mode-line-highlight
		 'local-map '(keymap
			      (mode-line keymap
					 (down-mouse-1 . image-next-frame)
					 (down-mouse-3 . image-previous-frame)))))))))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)

(declare-function image-flush "image.c" (spec &optional frame))

;;;###autoload
(define-derived-mode image-display-mode fundamental-mode "Display"
  "Mode to preview multiple images inside emacs buffer."
  :group 'image
  
  (unless (display-images-p)
    (error "Display does not support images"))

  (setq
   ;; todo:
   ;; mode-line-process image-display-mode-line-process
   cursor-type 'box
   truncate-lines t)
  
  (set-visited-file-name nil)

  (setq-local image-display--cursor-overlay (make-overlay 1 1))
  (overlay-put image-display--cursor-overlay
	       'face `(:box (:line-width ,image-display-border-width
					 :color ,image-display-cursor-color)
			    :background ,image-display-cursor-color))

  (image-display-insert-page)
  (image-mode-setup-winprops)

  ;; (read-only-mode 1)
  (setq line-spacing image-display-line-spacing)
  
  (let ((image (image-at-point))
	(msg1 (substitute-command-keys
	       "Type \\[todo:] to view the image as ")))
    (cond
     ((null image)
      (message "%s" (concat msg1 "an image.")))
     ((image-multi-frame-p image) 
      (message "%s" (concat msg1 "text.  This image has multiple frames.")))
     (t (message "%s" (concat msg1 "text.")))))
  
  (add-hook 'change-major-mode-hook
	    (lambda ()
	      ;; todo:remove display property
	      )
	    nil t))



;;; Image Mode

(defcustom image-mode-show-cursor t
  "Non-nil if the cursor should be shown in `image-mode'."
  :group 'image-mode
  :type 'boolean)

(defcustom image-mode-auto-display t
  "If non-nil, `image-mode' automatically displays images on initialization."
  :group 'image
  :type 'boolean
  :version "25.1")

(defvar image-mode-map
  (let ((map (make-sparse-keymap)))
    ;; todo
    ;; (define-key map "\C-c\C-c" 'image-mode-display-image)
    map)
  "Mode keymap for `image-mode'.")

;;;###autoload
(define-derived-mode image-mode fundamental-mode "Image Mode" 
  "Major mode for editing image files.
Key bindings:
\\{image-mode-map}"
  (image-mode--init-ring)
  (when image-mode-auto-display
    (image-display-mode)))

(defun ring-set (ring index item)
  (let* ((vec (cddr ring))
	 (ix (ring-index index (car ring) (cadr ring) (length vec))))
    (aset vec ix item)))

(defun image-mode--init-ring ()
  (let* ((file (buffer-file-name))
	 (dir (file-name-directory file))
	 (files (directory-files dir t (image-file-name-regexp) t))
	 (data-p (not (and file
			   (file-readable-p file)
			   (not (file-remote-p file))
			   (not (buffer-modified-p))
			   (not (and (boundp 'archive-superior-buffer)
				     archive-superior-buffer))
			   (not (and (boundp 'tar-superior-buffer)
				     tar-superior-buffer))))))
    ;; Add the current file to the list of images if necessary, in
    ;; case it does not match `image-file-name-regexp'.
    (unless (member file files)
      (push file files))
    (setq files (sort files 'string-lessp)
	  image-display-ring (ring-convert-sequence-to-ring files)
	  image-display-ring-index (ring-member image-display-ring file))
    (when data-p
      (ring-set image-display-ring image-display-ring-index
		(create-image
		 (string-make-unibyte
		  (buffer-substring-no-properties (point-min) (point-max)))
		 nil 'data)))))

(provide 'image-display)
