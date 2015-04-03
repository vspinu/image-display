;;; image-display.el --- Support for displaying multiple image files  -*- lexical-binding: t -*-
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

(defcustom image-display-show-cursor t
  "Non-nil if the cursor should be shown in `image-display-mode'."
  :group 'image-display
  :type 'boolean)

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

(defcustom image-display-default-layouts '(1 4 9)
  "Default geometry of the image-display page.
Can be a number or a cons of the form (rows . cols). A number
represents a total number of images per page. In this case the
number of rows and cols are computed sensibly based on the width
and height of the current window."
  :group 'image-display
  :type '(repeat
	  (choice (integer :tag "Images per Page"
			   :value 10)
		  (cons :tag "Rows x Cols"
			:value (2 . 2)
			integer integer))))

(defvar-local image-display-layouts nil)

(defcustom image-display-forward-commands '(forward-char
					    forward-list forward-sexp forward-word
					    right-word rigth-char subword-forward subword-right)
  "List of forward command recognized in `image-display-mode'")

(defcustom image-display-backward-commands '(backward-char
					     backward-list backward-sexp backward-word
					     left-word left-char subword-backward subword-left)
  "List of backward command recognized in `image-display-mode'")

(defcustom image-display-up-comands '(previous-line)
  "List of upward movement commands.")

(defcustom image-display-down-comands '(next-line)
  "List of downward movement commands.")

(defcustom image-display-scroll-up-comands '(scrool-up scroll-up-command)
  "List of upward scrolling commands.")

(defcustom image-display-scroll-down-comands '(scrool-down scroll-down-command)
  "List of down scrolling commands.")

(defcustom image-display-border-width (/ (frame-char-height) 2)
  "Border around images in `image-display-mode' buffers."
  :group 'image-display)


;;; HANDLERS

(defun image-display--pre-command-handler ()
  (cond
   ((memq this-command image-display-up-comands)
    (setq this-command 'image-display-previous-row))
   ((memq this-command image-display-down-comands)
    (setq this-command 'image-display-next-row))
   ((memq this-command image-display-scroll-up-comands)
    (setq this-command 'image-display-next-page))
   ((memq this-command image-display-scroll-down-comands)
    (setq this-command 'image-display-previous-page))))

(defvar-local image-display--last-point nil)
(defun image-display--post-command-handler (&optional force)
  ;; todo: set a meaningful mode-line-position
  (when (or force (not (eq image-display--last-point (point))))
    ;; move away from point-max and point-min
    (cond ((eq (point) (point-max))
	   (backward-char))
	  ((and (eq (point) (point-min))
		;; only needed when there is a left margin on first image
		(eq 0 (get-text-property (point) 'intangible)))
	   (forward-char)))
    (setq image-display--last-point (point))
    (when image-display--cursor-overlay
      (let ((inhibit-point-motion-hooks t)
	    (span (image-display--get-image-span (point))))
	(move-overlay image-display--cursor-overlay (car span) (cdr span))))
    (let ((image (image-at-point)))
      (when image
	(let ((file (or (image-get image :file)
			(image-get image :type))))
	  (rename-buffer (format "*ID[%s]*" (file-name-nondirectory file)) t ))))))



;;; PAGES

(defvar image-display-page-start-delimiter "^ISTART"
  "Sequence of characters that start a multi image portion of the buffer.")
(defvar image-display-page-end-delimiter "^IEND"
  "Sequence of characters that end a multi image portion of the buffer.")
(defvar-local image-display-current-layout nil)
(defvar-local image-display-current-geometry nil)

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

(defun image-display--area-loss (r c N W H)
  (let* ((side (min (/ H (float r)) (/ W (float c))))
	 (area (* side side))
	 (loss (+ (- (* W H) (* r c area))
		  (* (- (* c r) N) area))))
    loss))

(defun image-display--optimal-layout (N W H)
  (let ((min-loss 100000000)
	(layout))
   (cl-loop for c from 1 to N
	    for r = (ceiling (/ (float N) c))
	    for loss = (image-display--area-loss r c N W H)
	    when (< loss min-loss)
	    do (setq layout (cons r c)
		     min-loss loss))
   layout))

(defun image-display--compute-geometry (layout)
  "Return a list of the form (R C W H) from LAYOUT.
R and C are the number of rows and columns. W and H are the width
and height in pixels of the box to fit each image in. LAYOUT is
as in `image-display-default-layouts'."
  (let* ((wedges (window-inside-pixel-edges))
	 (wh (- (nth 3 wedges) (nth 1 wedges) (frame-char-height)))
	 (ww (- (nth 2 wedges) (nth 0 wedges) (frame-char-width)))
	 (layout (cond ((consp layout)
			(unless (and (numberp (car layout)) (numberp (cdr layout)))
			  (error "Rows and columns in layout specification must be numbers"))
			(when (or (< (car layout) 0) (< (cdr layout) 1))
			  (error "Rows and columns in layout specification must be positive."))
			layout)
		       ((numberp layout)
			(image-display--optimal-layout layout ww wh)))))
    (let* ((h (- (/ wh (car layout)) (frame-char-width)))
	   (w (- (/ ww (cdr layout)) (frame-char-height)))
	   (N (* (car layout) (cdr layout))))
      (list (car layout) (cdr layout) w h))))
;; (image-display--compute-geometry 6)

(defvar-local image-display--before-flip-column nil)
(defvar-local image-display--after-flip-column nil)

(defun image-display-next-page (&optional arg)
  ;; fixme: implement Nth previous page
  (interactive)
  (let* ((index (image-display-get index))
	 (orig-col (get-text-property (point) :id-col))
	 (col (or (and  (eq image-display--after-flip-column orig-col)
			image-display--before-flip-column)
		  orig-col))
	 (arg (or arg 1))
	 (fwd (>= arg 0))
	 (new-index (if fwd
			(1+ (cdr index))
		      (1- (car index)))))
    (image-display-insert-page nil new-index t)
    (image-display-goto-row-col (and fwd 1) col)
    (let ((new-col (get-text-property (point) :id-col)))
      (setq image-display--before-flip-column (when (< new-col col) col)
	    image-display--after-flip-column new-col))))

(defun image-display-previous-page (&optional arg)
  (interactive)
  (image-display-next-page (- (or arg 1))))

(defun image-display-previous-row ()
  ;; fixme: Nth row
  (interactive)
  (let ((row (get-text-property (point) :id-row))
	(col (get-text-property (point) :id-col)))
      (cond ((and row (= row 1))
	     (image-display-previous-page))
	    ((and row col)
	     (image-display-goto-row-col (1- row) col))
	    ;; should never happen
	    (t (previous-line)))))

(defun image-display-next-row ()
  (interactive)
  (let ((pos-eol (point-at-eol)))
    (if (or (= pos-eol (point-max))
	    (= pos-eol (1- (point-max))))
	(image-display-next-page)
      (let ((row (get-text-property (point) :id-row))
	    (col (get-text-property (point) :id-col)))
	(cond ((and row col)
	       (image-display-goto-row-col (1+ row) col))
	      ;; should never happen
	      (t (next-line)))))))

(defun image-display-insert-page (&optional ring index backp)
  "Display images from image ring associated with the page at POS."
  ;; if index is > ring-length, rotate
  (let* ((inhibit-read-only t)
	 (buffer-undo-list t)
	 (page-start (image-display-page-start))
	 (page-end (image-display-page-end page-start))
	 (ring  (or ring (image-display-get ring page-start)))
	 (rlen (ring-length ring))
	 (layout (or (image-display-get current-layout page-start)
		     (car (image-display-get layouts page-start))
		     (car (image-display-get default-layouts page-start))))
	 (geom (image-display--compute-geometry layout))
	 (N (* (car geom) (cadr geom)))
	 (index (or index (image-display-get index page-start)))
	 (interval (cond
		    ((numberp index)
		     (image-display--compute-index-interval index rlen N))
		    ((consp index) index)
		    (t (error "Index must be a number or cons of numbers"))))
	 (index-start (car interval))
	 (index-end (cdr interval))
	 ;; number of images actually inserted
	 (N (min N (1+ (- index-end index-start)))))

    (setq image-display-current-geometry geom)
    (image-display-put index (cons index-start index-end) page-start)

    ;; debug
    (switch-to-buffer (current-buffer))

    (with-silent-modifications
      (goto-char page-start)
      (delete-region page-start page-end)
      (cl-loop for ix from index-start to index-end
	       for i = (1+ (- ix index-start)) do
	       (let* ((img (ring-ref ring ix))
		      (img (if (stringp img)
			       (create-image img)
			     img))
		      (img (image-transform img
					    :resize image-display-auto-resize
					    :box (cddr geom)))
		      (size (image-size img t))
		      (side-px (/ (- (nth 2 geom) (car size)) 2.0))
		      (px-left (max 0 (floor side-px)))
		      (px-right (max 0 (ceiling side-px)))
		      (col (1+ (mod (1- i) (cadr geom))))
		      (row (1+ (/ (1- i) (cadr geom))))
		      (name (concat (or (image-get img :file)
					(number-to-string i))
				    " "))
		      (face `(:box (:line-width ,image-display-border-width :color ,(face-attribute 'default :background))))
		      (common-props `(face ,face :id-ix ,ix :id-col ,col :id-row ,row)))

		 (when (> px-left 0)
		   (insert (apply #'propertize " " 'intangible (1- i)
				  'display `(space :width (,px-left))
				  common-props)))
		 (insert-image img (apply #'propertize name :id i 'intangible i common-props) nil nil t)
		 (when (> px-right 0)
		   (insert (apply #'propertize " " 'intangible i 'display `(space :width (,px-right)) common-props)))
		 
		 (if (and (= col (cadr geom)))
		     (insert (propertize "\n" 'intangible i))
		   ;; emacs doesn't display horizontal border correctly, this is an awkward fix
		   (insert (propertize " " 'intangible i 'face face))))))
    (if (numberp index)
	(image-display-goto-image index)
      (goto-char page-start))))



;;; NAVIGATION

(defun image-display-goto-row-col (row col)
  ;; nil means last row/col
  (let* ((pstart (image-display-page-start))
	 (pend (image-display-page-end pstart)))

    ;; report: This reset is needed due to emacs bug. If new point is the same
    ;; as (point) goto-char jumps to a different position
    (goto-char pstart)

    (let ((row-pos (and row (image-display--next-property-equal pstart :id-row row))))
      (if row-pos
	  (let ((col-pos (and col (image-display--next-property-equal row-pos :id-col col))))
	    (if col-pos
		(goto-char col-pos)
	      ;; either COL is not specified, or there are less columns in this row than COL
	      (goto-char row-pos)
	      (move-end-of-line 1)
	      (backward-char 1)))
	;; last row
	(let ((col-pos (image-display--previous-property-equal pend :id-col col)))
	  (if col-pos
	      (goto-char col-pos)
	    (goto-char pend)
	    (backward-char 1)))))))

(defun image-display-goto-image (ix)
  (let* ((page-start (image-display-page-start))
	 (ix (mod ix (ring-length (image-display-get ring page-start))))
	 (index (image-display-get index page-start)))
    (unless (and (>= ix (car index))
		 (<= ix (cdr index)))
      (image-display-insert-page nil ix))
    (goto-char page-start) ; reset needed in emacs 24.4.93
    (goto-char (image-display--next-property-equal page-start :id-ix ix))
    (image-display--post-command-handler t)))



;;; RING MANAGEMENT

(defvar-local image-display-ring nil
  "Ring of images in the current buffer.")
(put 'image-display-ring 'permanent-local t)

(defvar-local image-display-index nil
  "Ring of images in the current buffer.")
(put 'image-display-index 'permanent-local t)

(defun image-display--compute-index-interval (index rlen N)
  ;; always compute index such that (mod index-start rlen) = 0 and INDEX is in
  ;; the interval
  (let* ((index (mod index rlen))
	 (index-start (* (/ index N) N))
	 (index-end (min (1- rlen) (1- (+ index-start N)))))
    (cons index-start index-end)))

(defmacro image-display-get (name &optional page-start)
  (declare (debug (symbolp &optional form))
	   ;; (gv-setter (lambda (val)
	   ;; 		`(image-display-put ,name ,val ,page-start)))
	   )
  (let ((kwd (intern (format ":id-%s" (symbol-name name))))
	(obj (intern (format "image-display-%s" (symbol-name name)))))
    `(or (get-text-property (or ,page-start (image-display-page-start))
			    ,kwd)
	 ,obj)))

(defmacro image-display-put (name val &optional page-start)
  (declare (debug (symbolp form &optional form)))
  (let ((kwd (intern (format ":id-%s" (symbol-name name))))
	(sym (intern (format "image-display-%s" (symbol-name name)))))
    `(let ((pstart (or ,page-start (image-display-page-start))))
       (if (and pstart (get-text-property pstart ,kwd))
	   (put-text-property page-start (1+ pstart) ,kwd ,val)
	 (set ',sym ,val)))))


;;; UTILS

(defun image-display--next-property-equal (pos prop val &optional limit)
  (if (equal val (get-text-property pos prop))
      pos
    (let ((pc-pos pos)
	  (limit (or limit (point-max))))
      (while (and (< pc-pos limit)
		  (setq pc-pos (next-single-property-change pc-pos prop nil limit))
		  (not (equal val (get-text-property pc-pos prop)))))
      (when (and pc-pos
		 (equal val (get-text-property pc-pos prop)))
	pc-pos))))

(defun image-display--previous-property-equal (pos prop val &optional limit)
  (let ((pc-pos pos)
	(limit (or limit (point-min))))
    (while (and (> pc-pos limit)
		(setq pc-pos (previous-single-property-change pc-pos prop nil limit))
		(not (equal val (get-text-property pc-pos prop)))))
    (if (and pc-pos
	     (equal val (get-text-property pc-pos prop)))
	pc-pos
      (if (equal val (get-text-property limit prop))
	  limit))))

(defun image-display--get-image-span (&optional pos)
  (let ((inhibit-point-motion-hooks t)
	(pos (or pos (point))))
    (cons
     (previous-single-property-change pos :id-ix nil (point-at-bol))
     (next-single-property-change pos :id-ix nil (point-at-eol)))))



;;; DISPLAY MODE

(defvar image-display--layout-history nil)

(defun image-display--read-layout (&optional prompt)
  (let ((str (completing-read (or prompt "Layout: ")
			      (delete-duplicates image-display--layout-history) nil nil nil
			      'image-display--layout-history
			      (cadr image-display--layout-history))))
    (if (string-match "^ *\\([0-9]+\\)\\(?:[ ,.]+\\([0-9]+\\)\\)? *$" str)
	(let ((n1 (match-string 1 str))
	      (n2 (match-string 2 str)))
	  (if n2
	      (cons (string-to-number n1) (string-to-number n2))
	    (string-to-number n1)))
      (setq image-display--layout-history (cdr image-display--layout-history))
      (image-display--read-layout "Wrong input. Please enter a number or a pair of numbers: "))))
      
(defun image-display-set-layout (&optional layout)
  (interactive "P")
  (when (and layout (not (numberp layout)))
    (setq layout (image-display--read-layout)))
  (if layout
      (image-display-put layouts image-display-default-layouts)
    (unless (image-display-get layouts)
      (image-display-put layouts image-display-default-layouts))
    (let ((layouts (image-display-get layouts)))
      (when (eq (car layouts) (image-display-get current-layout))
	(setq layouts (cdr layouts)))
      (setq layout (car layouts))
      (image-display-put layouts (cdr layouts))))
  (image-display-put current-layout layout)
  (image-display-refresh))

(defun image-display-refresh ()
  (interactive)
  (let ((ix (get-text-property (point) :id-ix)))
    ;; tothink: call imaged-display-mode instead?
    (image-display-put index ix)
    (image-display-insert-page)))

(defun image-display-as-text ()
  (interactive)
  (let ((img (image-at-point)))
    (if (null img)
	(error "No image at point")
      (let ((image-text-mode-auto-display nil)
	    (inhibit-read-only t)
	    (data (image-get img :data)))
	(if data
	    (progn
	      (erase-buffer)
	      (insert data)
	      (normal-mode))
	  (insert-file-contents (image-get img :file) t nil nil t))))))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)
(declare-function image-flush "image.c" (spec &optional frame))

(defvar image-display-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    ;; (define-key map "\C-c\C-c" 'image-display-toggle)
    (define-key map (kbd "SPC")       'image-display-next-page)
    (define-key map (kbd "S-SPC")     'image-display-previous-page)
    (define-key map (kbd "DEL")       'image-display-previous-page)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "f" 'forward-char)
    (define-key map "b" 'backward-char)
    (define-key map "l" 'image-display-set-layout)
    (define-key map "g" 'image-display-refresh)
    (define-key map "\C-c\C-c" 'image-display-as-text)
    (easy-menu-define image-display-menu map "Menu for Image Display mode."
      '("ImageDisplay"
	["Set Layout" image-display-set-layout :active t
	 :help "Cycle layout. Set with numeric prefix. Ask with C-u."]
	["Refresh" image-display-refresh :active t
	 :help "Refresh current display."]
	["Show as Text" image-display-as-text :active t
	 :help "Show image at point as text"]))
    map)
  "Mode keymap for `image-mode'.")

;;;###autoload
(define-derived-mode image-display-mode special-mode "Display"
  "Mode to preview multiple images inside emacs buffer."
  :group 'image
  
  (unless (display-images-p)
    (error "Display does not support images"))

  (setq
   ;; todo: mode-line-process image-display-mode-line-process
   cursor-type 'box
   truncate-lines t)
  
  (setq-local image-display--cursor-overlay (make-overlay 1 1))
  (setq image-display-layouts image-display-default-layouts)
  
  (let ((bg-color (face-attribute 'highlight :background)))
    (overlay-put image-display--cursor-overlay
		 'face `(:box (:line-width ,image-display-border-width
					   :color ,bg-color)
			      :background ,bg-color)))
  
  ;; ring and index have been setup
  (image-display-insert-page)
  ;; (image-mode-setup-winprops)
  (read-only-mode 1)

  (add-hook 'pre-command-hook 'image-display--pre-command-handler nil t)
  (add-hook 'post-command-hook 'image-display--post-command-handler nil t)

  (setq mode-line-process )
  
  ;; todo:remove display properties
  (add-hook 'change-major-mode-hook (lambda ()) nil t)

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
					   (down-mouse-3 . image-previous-frame))))))))))


;;; OTHER STUFF

(defcustom image-text-mode-auto-display t
  "If non-nil, `image-mode' automatically displays images on initialization."
  :group 'image
  :type 'boolean
  :version "25.1")

(defvar image-text-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-text-mode-display)
    map)
  "Mode keymap for `image-mode'.")

(define-derived-mode image-text-mode fundamental-mode "Image Mode" 
  "Major mode for editing image files.
Key bindings:
\\{image-mode-map}"
  (image-text-mode--init-ring))
(defalias 'image-mode 'image-text-mode)

(defun image-text-mode--display-maybe ()
  (when image-text-mode-auto-display
    (set-visited-file-name nil)
    (image-display-mode)))
(add-hook 'image-text-mode-hook 'image-text-mode--display-maybe)

(defun image-text-mode-display ()
  (interactive)
  (image-display-mode))

(defun ring-set (ring index item)
  (let* ((vec (cddr ring))
	 (ix (ring-index index (car ring) (cadr ring) (length vec))))
    (aset vec ix item)))

(defun image-text-mode--init-ring ()
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
	  image-display-index (ring-member image-display-ring file))
    (when data-p
      (ring-set image-display-ring image-display-index
		(create-image
		 (string-make-unibyte
		  (buffer-substring-no-properties (point-min) (point-max)))
		 nil 'data)))))

(defun insert-image2 (image &optional string area slice map)
  "Insert IMAGE into current buffer at point.
IMAGE is displayed by inserting STRING into the current buffer
with a `display' property whose value is the image.  STRING
defaults to a single space if you omit it.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
SLICE specifies slice of IMAGE to insert.  SLICE nil or omitted
means insert whole image.  SLICE is a list (X Y WIDTH HEIGHT)
specifying the X and Y positions and WIDTH and HEIGHT of image area
to insert.  A float value 0.0 - 1.0 means relative to the width or
height of the image; integer values are taken as pixel values.
If MAP is provided, it must be a keymap what will be used as
text property keymap. A special value of t means to use
`image-manipulation-map'"
  ;; Use a space as least likely to cause trouble when it's a hidden
  ;; character in the buffer.
  (unless string (setq string " "))
  (unless (eq (car-safe image) 'image)
    (error "Not an image: %s" image))
  (unless (or (null area) (memq area '(left-margin right-margin)))
    (error "Invalid area %s" area))
  (if area
      (setq image (list (list 'margin area) image))
    ;; Cons up a new spec equal but not eq to `image' so that
    ;; inserting it twice in a row (adjacently) displays two copies of
    ;; the image.  Don't try to avoid this by looking at the display
    ;; properties on either side so that we DTRT more often with
    ;; cut-and-paste.  (Yanking killed image text next to another copy
    ;; of it loses anyway.)
    (setq image (cons 'image (cdr image))))
  (when (eq map t)
    (setq map image-manipulation-map))
  (let ((start (point)))
    (insert string)
    (add-text-properties start (point)
			 `(display ,(if slice
					(list (cons 'slice slice) image)
				      image)
                                   rear-nonsticky (display)
                                   keymap ,map))))
(defalias 'insert-image 'insert-image2)

(provide 'image-display)
