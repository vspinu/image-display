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
(require 'image-manip)
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

(defcustom image-display-default-layouts '((2 . 4) 9 1)
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

(defvar-local image-display--layouts nil)

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
    (let ((ix (get-text-property (point) :id-ix)))
      (when ix
	(let* ((pstart (image-display-page-start-position))
	       (overlay (image-display-get cursor-overlay pstart)))
	  (when overlay
	    (let ((inhibit-point-motion-hooks t)
		  (span (image-display--get-image-span (point))))
	      (move-overlay overlay (car span) (cdr span))))
	  (unless (image-display-get delimited pstart)
	    (let ((name (car (ring-ref (image-display-get ring pstart) ix))))
	      (rename-buffer (format "*ID[%s]*" name) t ))))))))


;;; PAGES

(defvar image-display-delimited  nil
  "Non nil in delimited pages only.")
(defvar image-display-page-start-delimiter "IPAGE-START"
  "Sequence of characters that start a multi image portion of the buffer.")
(defvar image-display-page-end-delimiter "IPAGE-END"
  "Sequence of characters that end a multi image portion of the buffer.")
(defvar-local image-display-current-layout nil)
(defvar-local image-display-current-geometry nil)

(defun image-display-page-start-position (&optional pos)
  (save-excursion
    (goto-char (or (if (consp pos) (car pos) pos) (point-max)))
    (and (re-search-backward image-display-page-start-delimiter nil t)
	 (cons (match-beginning 0) (match-end 0)))))

(defun image-display-page-end-position (&optional pos)
  (save-excursion
    (goto-char (or (if (consp pos) (cdr pos) pos)
		   (point)))
    (and (re-search-forward image-display-page-end-delimiter nil t)
	 (cons (match-beginning 0) (match-end 0)))))

(defun image-display-ring-start-position (&optional pos)
  (let ((image-display-page-start-delimiter image-display-ring-start-delimiter)
	(image-display-page-end-delimiter image-display-ring-end-delimiter))
    (image-display-page-start-position pos)))

(defun image-display-ring-end-position (&optional pos)
  (let ((image-display-page-start-delimiter image-display-ring-start-delimiter)
	(image-display-page-end-delimiter image-display-ring-end-delimiter))
    (image-display-page-end-position pos)))

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

(defun image-display-fill-page (&optional index backp)
  "Display images from image ring associated with the page at POS."
  ;; if index is > ring-length, rotate
  (let* ((inhibit-read-only t)
	 (buffer-undo-list t)
	 (page-start (image-display-page-start-position))
	 (page-end (image-display-page-end-position page-start))
	 (ring (image-display-get ring page-start))
	 (rlen (ring-length ring))
	 (layout (or (image-display-get current-layout page-start)
		     (car (image-display-get -layouts page-start))
		     (car (image-display-get default-layouts page-start))
		     (car image-display-default-layouts)))
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
	 (N (min N (1+ (- index-end index-start))))
	 (retriever (image-display-get retriever page-start))
	 (pstart (or (cdr page-start) (point-min)))
	 (pend (or (car page-end) (point-max)))
	 ;; not used yet
	 (indent-str (if page-start
			 (save-restriction (goto-char (car page-start))
					   (make-string (current-column) ? ))
		       "")))

    (image-display-put index (cons index-start index-end) page-start)
    (image-display-put current-geometry geom)
    (image-display-put current-layout layout)

    ;; debug
    (switch-to-buffer (current-buffer))

    (with-silent-modifications
      (goto-char pstart)
      (delete-region pstart pend)
      (when (image-display-get delimited page-start)
	(insert "\n"))
      (cl-loop for ix from index-start to index-end
	       for i = (1+ (- ix index-start)) do
	       (let* ((img (ring-ref ring ix))
		      (img (apply retriever img))
		      (img (image-transform img
					    :resize image-display-auto-resize
					    :box (cddr geom)))
		      (size (image-size img t))
		      (side-px (/ (- (nth 2 geom) (car size)) 2.0))
		      (px-left (max 0 (floor side-px)))
		      (px-right (max 0 (ceiling side-px)))
		      (col (1+ (mod (1- i) (cadr geom))))
		      (row (1+ (/ (1- i) (cadr geom))))
		      (name (concat "image"
				    (or ;; (image-get img :file)
				     (number-to-string i))
				    " "))
		      (face `(:box (:line-width ,image-display-border-width
				    :color ,(face-attribute 'default :background))))
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
      (goto-char pstart))
    (let ((page-end (image-display-page-end-position page-start)))
      (put-text-property (car page-start) (cdr page-start) 'invisible 'image-display)
      (put-text-property (car page-end) (cdr page-end) 'invisible 'image-display)
      (add-text-properties (car page-start) (cdr page-end)
			   `(read-only t keymap ,image-display-mode-map)))))

(defun image-display-insert-page (ring index &optional retriever layouts current-layout)
  (let ((inhibit-read-only t)
	(start (point)))
    (insert image-display-page-start-delimiter "\n")
    (add-text-properties start (1+ start)
			 `(:id-index ,index :id--layouts nil :id-delimited t
				     :id-cursor-overlay ,(image-display-make-cursor-overlay)))
    (when ring
      ;; when nil, this page shares the global ring
      (put-text-property start (1+ start) :id-ring ring))
    (when retriever
      (put-text-property start (1+ start) :id-retriever retriever))
    (when layouts
      (put-text-property start (1+ start) :id-default-layouts layouts))
    (when current-layout
      (put-text-property start (1+ start) :id-current-layout current-layout))
    (save-excursion
      (indent-according-to-mode)
      (insert image-display-page-end-delimiter "\n"))
    (image-display--add-hooks)
    (image-display-fill-page)))



;;; INLINE RINGS
(defvar image-display-ring-start-delimiter "IRING-START")
(defvar image-display-ring-end-delimiter "IRING-END")
(defvar image-display-filename-pattern "\\[\\[%s\\]\\]")

(defun image-display--get-inline-ring (beg end)
  (save-excursion
    (goto-char beg)
    (let ((regexp (format image-display-filename-pattern
			  (format "\\(file:\\)?\\(?1:.*%s\\)"
				  (image-file-name-regexp t))))
	  files)
      (while (re-search-forward regexp end t)
	(push (match-string-no-properties 1) files))
      (setq files (mapcar (lambda (f) (list (file-name-nondirectory f) f))
			  (reverse files)))
      (ring-convert-sequence-to-ring files))))

(defun image-display--inline-page (&optional pos no-error)
  (let* ((rend (image-display-ring-end-position pos))
	 (rbeg (and rend (image-display-ring-start-position rend)))
	 (ring (if (and rend rbeg)
		   (image-display--get-inline-ring (cdr rbeg) (car rend))
		 (unless no-error
		   (error "No inline image ring at point")))))
    (when ring
      (goto-char (cdr rend))
      (image-display-insert-page ring 0)
      (add-to-invisibility-spec 'image-display)
      (add-text-properties (car rbeg) (cdr rend) '(invisible image-display read-only t)))))

(defun image-display-inline-pages (&optional start end)
  (interactive)
  (let ((start (or start (point-min)))
	(end (or end (point-max))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward image-display-ring-start-delimiter end t)
	(save-excursion (image-display--inline-page))))))



;;; NAVIGATION

(defun image-display-next-page (&optional arg)
  ;; fixme: Nth previous page
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
    (image-display-fill-page new-index t)
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
  ;; fixme: Nth row
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

(defun image-display-goto-row-col (row col)
  ;; nil means last row/col
  (let* ((pstart (or (cdr (image-display-page-start-position))
		     (point-min)))
	 (pend (or (car (image-display-page-end-position pstart))
		   (point-max))))

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
  (let* ((page-start (or (image-display-page-start-position)
			 (point-min)))
	 (ix (mod ix (ring-length (image-display-get ring page-start))))
	 (index (image-display-get index page-start))
	 (pstart (or (cdr-safe page-start)
		     page-start)))
    (unless (and (>= ix (car index))
		 (<= ix (cdr index)))
      (image-display-fill-page ix))
    (goto-char pstart) ; reset needed in emacs 24.4.93
    (goto-char (image-display--next-property-equal pstart :id-ix ix))
    (image-display--post-command-handler t)))



;;; RING MANAGEMENT

(defvar-local image-display-ring nil
  "Ring of images in the current buffer.")
(put 'image-display-ring 'permanent-local t)

(defvar-local image-display-index nil
  "Ring of images in the current buffer.")
(put 'image-display-index 'permanent-local t)

(defvar-local image-display-retriever 'image-display-default-retriever)
(put 'image-display-retriever 'permanent-local t)

(defmacro image-display-get (name &optional page-start)
  (declare (debug (symbolp &optional form))
	   ;; (gv-setter (lambda (val)
	   ;; 		`(image-display-put ,name ,val ,page-start)))
	   )
  (let ((kwd (intern (format ":id-%s" (symbol-name name))))
	(obj (intern (format "image-display-%s" (symbol-name name)))))
    `(let* ((pstart (or ,page-start (image-display-page-start-position)))
	    (pstart (if (consp pstart) (car pstart) pstart))
	    (props (and pstart (text-properties-at pstart))))
       (or (and props
		(plist-member props ,kwd)
		(plist-get props ,kwd))
	   ,obj))))

(defmacro image-display-put (name val &optional page-start)
  (declare (debug (symbolp form &optional form)))
  (let ((kwd (intern (format ":id-%s" (symbol-name name))))
	(sym (intern (format "image-display-%s" (symbol-name name)))))
    `(let* ((inhibit-read-only t)
	    (pstart (or ,page-start (image-display-page-start-position)))
	    (pstart (if (consp pstart) (car pstart) pstart))
	    (props (and pstart (text-properties-at pstart)))
	    (val ,val))
       (if (and props (plist-member props ,kwd))
	   (put-text-property pstart (1+ pstart) ,kwd val)
	 (set ',sym val)))))

(defun image-display-member (ring name)
  "Return index of item with name NAME from RING.
Like `ring-member' but compares RING element's car to NAME."
  (catch 'found
    (dotimes (ind (ring-length ring) nil)
      (when (equal name (car (ring-ref ring ind)))
	(throw 'found ind)))))

(defun image-display--compute-index-interval (index rlen N)
  ;; always compute index such that (mod index-start rlen) = 0 and INDEX is in
  ;; the interval
  (let* ((index (mod index rlen))
	 (index-start (* (/ index N) N))
	 (index-end (min (1- rlen) (1- (+ index-start N)))))
    (cons index-start index-end)))


;;; UTILS

(defun image-display-make-cursor-overlay ()
 (let ((overlay (make-overlay 1 1))
       (bg-color (face-attribute 'highlight :background)))
   (overlay-put overlay
		'face `(:box (:line-width ,image-display-border-width
					  :color ,bg-color)
			     :background ,bg-color))
   overlay))

(defun image-display--add-hooks ()
  (add-hook 'pre-command-hook 'image-display--pre-command-handler nil t)
  (add-hook 'post-command-hook 'image-display--post-command-handler nil t))

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
      (image-display-put -layouts image-display-default-layouts)
    (unless (image-display-get -layouts)
      (image-display-put -layouts image-display-default-layouts))
    (let ((-layouts (image-display-get -layouts)))
      (when (eq (car -layouts) (image-display-get current-layout))
	(setq -layouts (cdr -layouts)))
      (setq -layout (car -layouts))
      (image-display-put -layouts (cdr -layouts))))
  (image-display-put current-layout -layout)
  (image-display-refresh))

(defun image-display-refresh ()
  (interactive)
  (let ((ix (get-text-property (point) :id-ix)))
    ;; tothink: call imaged-display-mode instead?
    (image-display-put index ix)
    (image-display-fill-page)))

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
      '("ImgDisplay"
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

  (set-visited-file-name nil)
  (setq buffer-auto-save-file-name nil ;; set-visited-file-name doesn't set this in emacs24
  	change-major-mode-with-file-name nil)

  (setq
   ;; todo: mode-line-process image-display-mode-line-process
   cursor-type 'box
   truncate-lines t)
  
  (setq image-display-layouts image-display-default-layouts)
  (setq-local image-display-cursor-overlay (image-display-make-cursor-overlay))
  
  ;; ring and index have been setup
  (image-display-fill-page)
  ;; (image-mode-setup-winprops)
  (read-only-mode 1)

  (image-display--add-hooks)
  
  ;; todo:remove display properties
  (add-hook 'change-major-mode-hook (lambda ()) nil t))

;; (defvar image-display-mode-line-process
;;   '(:eval
;;     (let* ((image (image-at-point))
;; 	   (animated (image-multi-frame-p image)))
;;       (concat " "
;; 	      (when animated
;; 		(propertize
;; 		 (format "[%s/%s]"
;; 			 (1+ (image-current-frame image))
;; 			 (car animated))
;; 		 'help-echo "Frames\nmouse-1: Next frame\nmouse-3: Previous frame"
;; 		 'mouse-face 'mode-line-highlight
;; 		 'local-map '(keymap
;; 			      (mode-line keymap
;; 					 (down-mouse-1 . image-next-frame)
;; 					 (down-mouse-3 . image-previous-frame)))))))))


;;; RING CONSTRUCTORS
;; todo: for tar, archive and remotes, store the already loaded image 

(defun image-display--create-image (name data)
  (let* ((data (string-make-unibyte data))
	 (type (or (image-type-from-data data)
		   (image-type-from-file-name name))))
    (create-image data type t)))

(defun image-display-init-directory-ring (&optional cur-file)
  (let* ((files (directory-files default-directory t (image-file-name-regexp) t)))
    ;; Add the current file to the list of images if necessary, in
    ;; case it does not match `image-file-name-regexp'.
    (when cur-file
      (unless (member cur-file files)
	(push cur-file files)))
    ;; todo: deal with fileless buffers somehow
    ;; (when data-p
    ;;   (ring-set image-display-ring image-display-index
    ;; 		(create-image
    ;; 		 (string-make-unibyte
    ;; 		  (buffer-substring-no-properties (point-min) (point-max)))
    ;; 		 nil 'data)))
    (let* ((alist (mapcar (lambda (f) (list (file-name-nondirectory f) f))
			  (sort files 'string-lessp)))
	   (new-ring (ring-convert-sequence-to-ring alist)))
      (image-display-put ring new-ring)
      (when cur-file
	(let ((name (file-name-nondirectory cur-file)))
	  (image-display-put index (image-display-member new-ring name)))))))

(declare-function tramp-handle-insert-file-contents "tramp" (filename &optional visit beg end replace))
(defun image-display-default-retriever (name obj &rest ignored)
  (cond ((stringp obj)
	 (if (not (file-remote-p obj))
	     (create-image obj)
	   (require 'tramp)
	   (with-temp-buffer
	     (tramp-handle-insert-file-contents obj)
	     (image-display--create-image obj (buffer-string)))))
	((and (consp obj) (eq (car obj) 'image))
	 obj)
	(t (error "default retriever can handle only file names and images."))))

;; TAR
(defvar tar-superior-buffer)
(defvar tar-superior-descriptor)
(declare-function tar-header-name "tar-mode" (CL-X))
(declare-function tar--extract "tar-mode" (descriptor))

(defun image-display-init-tar-ring (&optional cur-descr)
  (let* ((regexp (image-file-name-regexp))
	 (ring (ring-convert-sequence-to-ring
		(with-current-buffer tar-superior-buffer
		  (cl-loop for hdr in tar-parse-info
			   for name = (tar-header-name hdr)
			   when (string-match-p regexp name)
			   collect (list name hdr))))))
    (image-display-put ring ring)
    (image-display-put retriever #'image-display-tar-retriever)
    (when cur-descr
      (let ((name (tar-header-name cur-descr)))
	(image-display-put index (image-display-member ring name))))))

(defun image-display-tar-retriever (file descriptor)
  (let ((buff (with-current-buffer tar-superior-buffer
		(tar--extract descriptor))))
    (prog1 (with-current-buffer buff
	     (image-display--create-image file (buffer-string)))
      (kill-buffer buff))))

;; ARCHIVE
(defvar archive-superior-buffer)
(defvar archive-subfile-mode)
(defvar archive-file-name-coding-system)
(declare-function archive-*-extract "arc-mode" (archive name command))

(defun image-display-init-archive-ring (&optional cur-descr)
  (let* ((regexp (image-file-name-regexp))
	 (ring (ring-convert-sequence-to-ring
		(with-current-buffer archive-superior-buffer
		  (cl-loop for descr across archive-files
			   for name = (aref descr 0)
			   when (string-match-p regexp name)
			   collect (list name descr))))))
    (image-display-put ring ring)
    (image-display-put retriever #'image-display-archive-retriever)
    (when cur-descr
      (let ((name (aref cur-descr 0)))
	(image-display-put index (image-display-member ring name))))))

(defun image-display-archive-retriever (file descr)
  ;; very short version of `archive-extract'
  (with-current-buffer archive-superior-buffer
    (let ((archive (buffer-file-name))
	  (archive-buffer (current-buffer))
	  (extractor (archive-name "extract"))
	  (name (aref descr 0))
	  (arcdir default-directory)
	  (file-name-coding archive-file-name-coding-system))
      (with-temp-buffer
	(make-local-variable 'archive-superior-buffer)
	(setq default-directory arcdir
	      archive-superior-buffer archive-buffer
	      archive-subfile-mode descr
	      archive-file-name-coding-system file-name-coding)
	(let ((coding-system-for-read 'no-conversion))
	  (if (fboundp extractor)
	      (funcall extractor archive name)
	    (archive-*-extract archive name
			       (symbol-value extractor))))
	(image-display--create-image file (buffer-string))))))


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
  ;; todo: reuse loaded image data in the current file
  (cond
   ;; tar
   ((and (boundp 'tar-superior-buffer) tar-superior-buffer)
    (image-display-init-tar-ring  tar-superior-descriptor))
   
   ;; archive
   ((and (boundp 'archive-superior-buffer) archive-superior-buffer)
    (image-display-init-archive-ring archive-subfile-mode))
   
   ;; directory
   (default-directory
     (let* ((file (buffer-file-name))
	    (file (and (file-readable-p file) file)))
       (image-display-init-directory-ring file)))
   (t (error "Cannot create image ring"))))

(defalias 'image-mode 'image-text-mode)

(defun image-text-mode--display-maybe ()
  (when (and image-text-mode-auto-display
	     (display-images-p))
    (image-display-mode)))
(add-hook 'image-text-mode-hook 'image-text-mode--display-maybe)

(defun image-text-mode-display ()
  (interactive)
  (image-display-mode))

(defun ring-set (ring index item)
  (let* ((vec (cddr ring))
	 (ix (ring-index index (car ring) (cadr ring) (length vec))))
    (aset vec ix item)))

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

(defun image-file-name-regexp2 (&optional no-empty-end)
  "Return a regular expression matching image-file filenames.
If NO-EMPTY-END is non-nil, don't append empty string terminator
\\' at the end of the regular expression."
  (let ((exts-regexp
	 (and image-file-name-extensions
	      (concat "\\."
		      (regexp-opt (nconc (mapcar #'upcase
						 image-file-name-extensions)
					 image-file-name-extensions)
				  t)
		      (unless no-empty-end "\\'")))))
    (if image-file-name-regexps
	(mapconcat 'identity
		   (if exts-regexp
		       (cons exts-regexp image-file-name-regexps)
		     image-file-name-regexps)
		   "\\|")
      exts-regexp)))
(defalias 'image-file-name-regexp 'image-file-name-regexp2)


(provide 'image-display)
