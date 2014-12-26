;;; image-transform.el --- Support for image transformations  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2014 Free Software Foundation, Inc.
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
;;  Documentation is in `image-transform` and `image-transform-backends`
;;
;;; TODO:
;;
;;  Make `image-tranform:convert' operate on :data
;; 
;;  Implement multi-image transformations by allowing IMAGE argument to
;;  `image-transform' to be a list of images.
;;
;;; Code:

(require 'image)
(eval-when-compile
  (require 'cl-lib))

(defcustom image-transform-backends '(native convert)
  "List of backends which are tried in turn by `image-transform'.

The `native' backend uses internal Emacs ImageMagick support;
`convert' backed uses external ImageMagick 'convert' utility.  If
Emacs was not compiled with ImageMagick support `native' backend
is ignored.

Each backend 'BACKEND' consists of the following components:

 1) Transformation function 'image-transform:BACKEND' that takes as
    input an image and transform specs.  Currently implemented
    functions are `image-transform:native' and
    `image-transform:convert'.
 
 2) Supported transforms for each backend are stored in
    'image-transform-features:BACKEND' list of lists of of the
    form (FEATURE DOC TYPE), where:

    - FEATURE is a keyword naming a transformation.
   
    - DOC is a string describing the feature.
   
    - TYPE is a symbol such that 'image-transform-spec:TYPE'
      is a declared function that accepts KEY and VALUE arguments
      and returns a list of one or more canonical pairs (NEWKEY
      NEWVALUE) to be passed to the appropriate backend.  NEWVALUE
      is a string that can be passed directly to ImageMagick
      'convert' program.  For a complete list, see
      http://www.imagemagick.org/script/command-line-options.php

      Most common examples of normalizers are
      `image-transform-spec:number' and
      `image-transform-spec:geometry'.

    See `image-transform-features:native' and
    `image-transform-features:convert' for available features."
  :group 'image
  :type '(repeat symbol))

(defcustom image-transform-convert-program "convert"
  "Name of the \"convert\" program."
  :group 'image
  :type 'string)

(defvar image-transform-verbose t
  "If non-nil, display conversion messages.")

(define-error 'next-backend "Backend failed" 'error)


;;; Public Utilities

(defun image-get-transforms (&optional image)
  "Return transformation chain of the IMAGE.
IMAGE defaults to `image-at-point'."
  (interactive)
  (setq image (or image (image-at-point)))
  (or (image-get image :transforms)
      '(tr)))


;;; Internal Utilities

(defun image-tr--delete-properties (list props)
  "Remove PROPS from LIST destructively.
LIST is like plist, but possibly with repeated keys. Return
transformed LIST."
  (let ((p list))
    (while p
      (if (member (cadr p) props)
          (setcdr p (nthcdr 3 p))
        (setq p (cdr p))))
    list))

(defun image-tr--delete-transforms (image transforms)
  "Remove IMAGE's TRANSFORMS destructively."
  (image-tr--delete-properties (image-get image :transforms) transforms)
  image)

(defun image-tr--modify-transform (image N new-val)
  "Modify IMAGE N's transform destructively."
  (let ((trs (cdr (image-get-transforms image)))
	(i N))
    (while (> i 1)
      (setq trs (cddr trs)
	    i (1- i)))
    (if trs
	(setcdr trs (cons new-val (cddr trs)))
      (error "There are fewer transforms than %s" N))
    image))


(defun image-tr--osize (image)
  (let ((osize (image-get image :osize)))
    (unless osize
      (setq osize (image-size image t))
      (image-put image :osize osize))
    osize))

(defun image-tr--rescale (image sw &optional sh)
  ;; used by native backend
  (setq sh (or sh sw))
  (unless (and (= sw 100)
               (= sh 100))
    (let ((sw (/ sw 100.0))
          (sh (/ sh 100.0))
          (uw (image-get image :width))
          (uh (image-get image :height)))
      (let ((size (image-tr--osize image)))
        (if (or uw uh)
            (if (= sw sh)
                ;; only one could have been supplied, keep it
                (progn (when uw
                         (image-put image :width (floor (* sw uw))))
                       (when uh
                         (image-put image :height (floor (* sh uh)))))
              (image-put image
                         :width (floor (* sw (or uw
                                                 (* (/ (car size) (float (cdr size))) uh)))))
              (image-put image
                         :height (floor (* sh (or uh
                                                  (* (/ (cdr size) (float (car size))) uw))))))
          (image-put image :width (floor (* sw (car size))))
          (when (/= sw sh)
            (image-put image :height (floor (* sh (cdr size))))))))))

(defvar image-tr--right-angle-fudge 0.0001
  "Snap distance to a multiple of a right angle.
There's no deep theory behind the default value, it should just
be somewhat larger than ImageMagick's MagickEpsilon.")

;; These two functions are slightly adapted from the old image-mode.el and were
;; authored by by Wolfgang Jenkner <wjenkner@inode.at>
(defsubst image-tr--get-rotated-width (width height rotation)
  "Return the bounding box width of a rotated WIDTH x HEIGHT rectangle.
ROTATION is the rotation angle in  degrees."
  (let ((angle (degrees-to-radians rotation)))
    ;; Assume, w.l.o.g., that the vertices of the rectangle have the
    ;; coordinates (+-w/2, +-h/2) and that (0, 0) is the center of the
    ;; rotation by the angle A.  The projections onto the first axis
    ;; of the vertices of the rotated rectangle are +- (w/2) cos A +-
    ;; (h/2) sin A, and the difference between the largest and the
    ;; smallest of the four values is the expression below.
    (+ (* width (abs (cos angle))) (* height (abs (sin angle))))))

;; The following comment and code snippet are from
;; ImageMagick-6.7.4-4/magick/distort.c

;;    /* Set the output image geometry to calculated 'best fit'.
;;       Yes this tends to 'over do' the file image size, ON PURPOSE!
;;       Do not do this for DePolar which needs to be exact for virtual tiling.
;;    */
;;    if ( fix_bounds ) {
;;      geometry.x = (ssize_t) floor(min.x-0.5);
;;      geometry.y = (ssize_t) floor(min.y-0.5);
;;      geometry.width=(size_t) ceil(max.x-geometry.x+0.5);
;;      geometry.height=(size_t) ceil(max.y-geometry.y+0.5);
;;    }

;; Other parts of the same file show that here the origin is in the
;; left lower corner of the image rectangle, the center of the
;; rotation is the center of the rectangle and min.x and max.x
;; (resp. min.y and max.y) are the smallest and the largest of the
;; projections of the vertices onto the first (resp. second) axis.
(defun image-tr--get-rotated-size (width height length &optional rotation)
  "Return (w . h) so that a rotated w x h image has exactly width LENGTH.
The ROTATION angle defaults 0.

Write W for WIDTH and H for HEIGHT.  Then the w x h rectangle is
an \"approximately uniformly\" scaled W x H rectangle, which
currently means that w is one of floor(s W) + {0, 1, -1} and h is
floor(s H), where s is a scale factor. The value of ROTATION may
be replaced by a slightly different angle.  Currently this is
done for values close to a multiple of 90, see
`image-tr--right-angle-fudge'."
  (setq rotation (or rotation 0.0))
  (cond ((< (abs (- (mod (+ rotation 90) 180) 90))
            image-tr--right-angle-fudge)
         (cl-assert (not (zerop width)) t)
         (cons length nil))
        ((< (abs (- (mod (+ rotation 45) 90) 45))
            image-tr--right-angle-fudge)
         (cl-assert (not (zerop height)) t)
         (cons nil length))
        (t
         (let (scale)
           (cl-assert (not (and (zerop width) (zerop height))) t)
           (setq scale
                 (/ (float (1- length))
                    (image-tr--get-rotated-width width height rotation)))
           ;; Assume we have a w x h image and an angle A, and let l =
           ;; l(w, h)) = w |cos A| + h |sin A|, which is the actual width
           ;; of the bounding box of the rotated image, as calculated by
           ;; `image-tr--get-rotated-width'.  The code snippet quoted above
           ;; means that ImageMagick puts the rotated image in
           ;; a bounding box of width L = 2 ceil((w+l+1)/2) - w.
           ;; Elementary considerations show that this is equivalent to
           ;; L - w being even and L-3 < l(w, h) <= L-1.  In our case, L is
           ;; the given `length' parameter and our job is to determine
           ;; reasonable values for w and h which satisfy these
           ;; conditions.
           (let ((w (floor (* scale width)))
                 (h (floor (* scale height))))
             ;; Let w and h as bound above.  Then l(w, h) <= l(s W, s H)
             ;; = L-1 < l(w+1, h+1) = l(w, h) + l(1, 1) <= l(w, h) + 2,
             ;; hence l(w, h) > (L-1) - 2 = L-3.
             (cons
              (cond ((= (mod w 2) (mod length 2))
                     w)
                    ;; l(w+1, h) >= l(w, h) > L-3, but does l(w+1, h) <=
                    ;; L-1 hold?
                    ((<= (image-tr--get-rotated-width (1+ w) h rotation)
                         (1- length))
                     (1+ w))
                    ;; No, it doesn't, but this implies that l(w-1, h) =
                    ;; l(w+1, h) - l(2, 0) >= l(w+1, h) - 2 > (L-1) -
                    ;; 2 = L-3.  Clearly, l(w-1, h) <= l(w, h) <= L-1.
                    (t
                     (1- w)))
              h))))))


;;; Parsers
;; Parser converts a 'convert' style string specification into a meaningful
;; elisp representation.

(defun image-transform-parse:number (str)
  "Parse a numeric string STR.
String can contain leading or trailing spaces.  If STR is a number
simply return it."
  (if (numberp str)
      str
    (when (string-match "^ *\\([+-]?[0-9.]+\\) *$" str)
      (string-to-number (match-string 1 str)))))

(defun image-transform-parse:scale (str)
  "Parse a numeric string STR as scale.
Like `image-transform-parse:number' but expects % at the end of
the number."
  (if (numberp str)
      str
    (when (string-match "^ *\\([0-9.]+\\)% *$" str)
      (string-to-number (match-string 1 str)))))

(defun image-transform-parse:geometry (geom)
  "Parse ImageMagick geometry specification.

X can be either \"x\",\"X\" or \" \" (space).

scale%  	Height and width both scaled by specified percentage.
xscaleXyscale% 	Height and width individually scaled by specified percentages.
width   	Width given, height automagically selected to preserve aspect ratio.
Xheight 	Height given, width automagically selected to preserve aspect ratio.
widthXheight 	Maximum values of height and width given, aspect ratio preserved.
widthXheight^ 	Minimum values of width and height given, aspect ratio preserved.
widthXheight! 	Width and height emphatically given, original aspect ratio ignored.
widthXheight> 	Shrinks an image with dimension(s) larger than the width and height arguments.
widthXheight< 	Enlarges an image with dimension(s) smaller than the width and height arguments.
area@   	Resize image to have specified area in pixels. Aspect ratio is preserved.

Return a list of the form (W H o) where o is an operator giving
an additional meaning to W and H. Can be ^ ! > < or @.

Offset geometry from \"convert\" specifications is not yet
supported. See http://www.imagemagick.org/script/command-line-processing.php#geometry"
  (when (and (stringp geom)
             (string-match
              "^ *\\([0-9.]+\\)?[xX ]?\\([0-9.]+\\)?\\([@><^!%]\\)? *$"
              geom))
    (let ((W (match-string 1 geom))
          (H (match-string 2 geom))
          (o (match-string 3 geom)))
      (list (and W (string-to-number W))
            (and H (string-to-number H))
            o))))


;;; Normalizers

;; convert backend can accept a multitude of string inputs. 
(defvar image-transform-accept-unparsed nil
  "Non-nil when numeric writers should accepts non passable strings as a valid input.")

(defun image-transform-spec:number (key value)
  "Basic processor of numeric transform specifications.

KEY names the transformation.  VALUE must be a number or a string
that could be converted into a number.  If the conversion has
failed and `image-transform-accept-unparsed' is nil, signal
'next-backend."
  (setq value
        (pcase value
          ((or (pred null)
               (pred numberp)) value)
          ((pred stringp)
           (or (image-transform-parse:number value)
               (if image-transform-accept-unparsed
                   value
                 (signal 'next-backend
                         (list (format "Cannot parse %s value" key))))))
          (_ (signal 'next-backend
                     (list (format "Invalid type of argument %s" key))))))
  (list key (if (numberp value)
                (number-to-string value)
              value)))

(defalias 'image-transform-spec:degrees 'image-transform-spec:number)
(defalias 'image-transform-spec:value 'image-transform-spec:number)

(defun image-transform-spec:scale (tr value)
  "Process scale argument of transformation TR.
VALUE must be a number or string of the form \"N%\" where N is a
number, giving the percentage by which to scale the image.

For \"convert\" backend scale can take additional geometry
specifications."
  (setq value
        (pcase value
          ((or (pred null)
               (pred numberp)) value)
          ((pred stringp)
           (or (image-transform-parse:scale value)
               (if image-transform-accept-unparsed
                   value
                 (signal 'next-backend
                         (list (format "Cannot parse %s value" tr))))))
          (_ (signal 'next-backend
                     (list (format "Invalid type of argument %s" tr))))))
  (list tr (if (numberp value)
                (concat (number-to-string value) "%")
              value)))

(defun image-transform-spec:boolean (key value)
  "Simply return (KEY VALUE) list."
  (list key value))

(defun image-transform-spec:geometry (tr value)
  "Normalize geometry VALUE for transformation TR.

VALUE can be

 - numeric - interpreted as width.

 - string - interpreted as \"convert\" program specification. It
   is parsed by `image-transform-parse:geometry' for 'native'
   backend and is passed as is to 'convert' backend.

 - cons (W . H) or list (W H) - rescale the image maximally such
   that new image fits into (W . H) box. Aspect ratio
   preserved. Equivalent to WxH \"convert\" specification.

 - list (W H o) - Similar to \"WxHo\" convert geometry
   specification where 'o' can be \"!\",\"%\",\"^\",\"@\",\">\"
   or \"<\".

For example, to force the new dimensions of the image you should
supply (list W H \"!\")."

  ;; If `image-transform-accept-unparsed' is non-nil, accepts strings that
  ;; don't match the above pattern. Otherwise pass to next backend by
  ;; signaling 'next-backend.

  (setq value
        (pcase value
          ((pred null) value)
          ((pred numberp) (number-to-string value))
          (`(,w ,h ,o . nil) (concat (and w (number-to-string w)) "x"
                                     (and h (number-to-string h)) o))
          ;; (`(,w . nil) (number-to-string w))
          ((or `(,w ,h . nil) `(,w . ,h))
           (concat (and w (number-to-string w)) "x"
                   (and h (number-to-string h))))
          ((pred stringp)
           (if (or (image-transform-parse:geometry value)
                   image-transform-accept-unparsed)
               value
             (signal 'next-backend
                     (list (format "Cannot parse %s geometry argument" tr)))))
          (_ (signal 'next-backend
                     (list (format "Invalid %s geometry specification" tr))))))
  (list tr value))

(defun image-transform-spec:choice (TR value &rest choices)
  "For transformation TR, check if VALUE is in CHOICES list.
Return '(TR value) if true, and signal 'next-backend error
otherwise."
  (if (member (intern value) choices)
      (list TR value)
    (signal 'next-backend
            (list (format "%s is not a valid value of option %s" value TR)))))



;;; Transform API utilities

(defun image-tr--available-features (backend)
  "Get the names of all available features for BACKEND."
  (cl-mapcan (lambda (list) (mapcar #'car (cdr list)))
	     (symbol-value (intern (format "image-transform-features:%s" backend)))))

(defun image-tr--get-feature (name features)
  "Retrieve feature with NAME from an alist of lists of FEATURES.
If FEATURES is a symbol, it is interpreted as a backend name."
  (let ((feats (if (symbolp features)
		   (symbol-value (intern (format "image-transform-features:%s" features)))
		 features)))
    (cl-loop for flist in feats
	     for f = (assoc name (cdr flist))
	     if f return f)))

(defun image-tr--unsupported-features (specs backend)
  "Return unsupported features from SPECS by BACKEND.
SPECS are as in `image-transform'.  BACKEND is a symbol or a
string."
  (let* ((features (cl-loop for s in specs if (keywordp s) collect s))
	 (available (symbol-value
		     (intern (concat "image-transform-features:"
				     (if (symbolp backend)
					 (symbol-name backend)
				       backend))))))
    (cl-loop for f in features
             unless (image-tr--get-feature f available)
             collect f)))

(defun image-tr--check-unsupported-features (image newspecs backend)
  "Signal 'next-backend if any of IMAGE's specs or NEWSPECS are unsupported by BACKEND.
NEWSPECS are as SPECS in `image-transform'."
  (let* ((trlist (image-get-transforms image))
         (un-specs (image-tr--unsupported-features newspecs backend))
         (un-tr (image-tr--unsupported-features (cdr trlist) backend)))
    (when un-specs
      (signal 'next-backend
              (list (format "Unsupported features: %s" un-specs))))
    (when un-tr
      (signal 'next-backend
              (list (format "Unsupported existing transforms: %s" un-tr))))))

(defun image-tr--normalize-specs (specs backend)
  "Normalize SPECS given the spec types declared in the BACKEND."
  (let ((available (symbol-value
		    (intern (concat "image-transform-features:"
				    (symbol-name backend))))))
    (cl-loop for s on specs by 'cddr append
	     (let* ((el (image-tr--get-feature (car s) available))
		    (type (nth 2 el))
		    (reader (intern (concat "image-transform-spec:"
					    (if (listp type)
						(symbol-name (car type))
					      (symbol-name type))))))
	       (unless el
		 (error "No reader for %s in %s backend" (car s) backend))
	       (if (fboundp reader)
		   (if (listp type)
		       (apply reader (car s) (cadr s) (cdr type))
		     (funcall reader (car s) (cadr s)))
		 (list (car s) (cadr s)))))))

(defun image-tr--add-transforms (trs newtrs)
  "Destructively populate TRS with NEWTRS transforms.
Return new TRS.  Both TRS and NEWTRS are list of pairs :TR VALUE,
with possibly repeated keys.  If VALUE is nil, all :TR key-values
are removed from TRS."
  (cl-loop for s on newtrs  by 'cddr do
           (let ((kwd (car s))
                 (val (cadr s)))
             (if (null val)
                 ;; API: remove null properties 
                 (image-tr--delete-properties trs (list kwd))
               (setq trs (append trs (list kwd val))))))
  trs)

(defun image-tr--adjust-specs-for-fit (image newspecs)
  "Adjust IMAGE's specs by processing :resize spec from NEWSPECS.
Return a transformed NEWSPECS list."
  (let ((resize (cadr (memq :resize newspecs))) )
    (when (and resize
               (symbolp resize))
      (let* ((rotate (cadr (memq :rotate newspecs)))
             (orot (image-get image :rotation))
             (box (or (cadr (memq :box newspecs))
                      (selected-window)))
             ;; Note: `image-size' looks up and thus caches the
             ;; untransformed image. There's no easy way to prevent
             ;; that. VS[17-07-2013]: This seems not to be true
             ;; anymore, `image-size' does return the size of
             ;; transformed image.
             (size (image-tr--osize image))
             (newrot (float (mod (+ (or rotate 0.0) (or orot 0.0)) 360)))
             (newsize (cons (image-tr--get-rotated-width
                             (car size) (cdr size) newrot)
                            (image-tr--get-rotated-width
                             (cdr size) (car size) newrot))))

        (unless (member resize '(nil fit fit-if-large fit-width
                                     fit-height fit-stretch))
          (error "Invalid :resize argument"))
        
        (plist-put newspecs :resize
                   (let* ((wsize (if (windowp box)
				     (let ((wedges (window-inside-pixel-edges box)))
				       (cons (- (nth 2 wedges)
						(nth 0 wedges))
					     (- (nth 3 wedges)
						(nth 1 wedges))))
				   box))
                          (resize (if (and (eq resize 'fit-if-large)
                                           (or (> (car newsize) (car wsize))
                                               (> (cdr newsize) (cdr wsize))))
                                      'fit
                                    resize))
                          (resize (if (eq resize 'fit)
                                      (if (< (/ (float (car wsize)) (cdr wsize))
                                             (/ (float (car newsize)) (cdr newsize)))
                                          'fit-width
                                        'fit-height)
                                    resize)))

                     (cond
                      ((eq resize 'fit-stretch)
                       (let ((res (image-tr--get-rotated-size
                                   (car wsize) (cdr wsize) (car wsize) newrot)))
                         ;; fixme: stretching doesn't work correctly with rotation
                         (when (null (car res))
                           (setcar res (car wsize)))
                         (when (null (cdr res))
                           (setcdr res (cdr wsize)))
                         (list (car res) (cdr res) "!")))
                      ((eq resize 'fit-width)
                       (let ((res (image-tr--get-rotated-size
                                   (car size) (cdr size) (car wsize) newrot)))
                         ;; don't provide both W and H unnecessarily
                         (list (car res) (unless (car res) (cdr res)))))
                      ((eq resize 'fit-height)
                       (let ((res (image-tr--get-rotated-size
                                   (cdr size) (car size) (cdr wsize) newrot)))
                         (list (unless (car res) (cdr res)) (car res))))))))))
  (image-tr--delete-properties newspecs '(:box)))



;;; Transform API

;;;###autoload
(defun image-transform (image &rest specs)
  "Transform IMAGE by applying transformation SPECS.
IMAGE can be a list of images, in which case SPECS are applied to
all the images in turn.

Transformation backends listed in `image-transform-backends' are
tried in turn and the first suitable backend is applied.  If none
of the backends can be applied an error is thrown.

SPECS is a list of key-value pairs giving the name and parameters
of the transformation.  See `image-transform-features:native' and
`image-transform-features:convert' for lists of supported
features.

Specs's values are either strings encoded with image 'convert'
conventions or elisp objects that are normalized to canonical
string form with `image-transform-spec:TYPE' functions, where
TYPE is the type of the spec.  Examples of normalizers are
`image-transform-spec:geometry', `image-transform-spec:number',
`image-transform-spec:scale' and `image-transform-spec:choice'.

All the transformations applied so far are stored as part of the
Emacs image data structure.  Each time a new transformation is
applied, the whole transformation chain is re-applied to the
original image.  This ensures that internal optimization of the
'convert' backend can be used and the image doesn't unnecessarily
loose quality on repeated transformation.

This function acts destructively.  Use `copy-list' to avoid
modifying existing images.

Several specs are treated specially or are pre-processed before
being dispatched to the backend functions:

  :backend - enforce a specific backend.

  :resize can be:
   - a number, giving new width of the image
   - a cons, giving the size (w x h) in pixels.
   - a symbol:
     *`fit' - maximally scale IMAGE to fit into window
     *`fit-height' - fit the image to window's height
     *`fit-width' - fit the image to window's width
     *`fit-stretch' - stretch the image to fit to both height and
      width of the window
 
  :box - surrounding box specification used when :resize is a
  symbol.  Can be a window or a cons cell of the form (W . H)
  specifying the size of the surrounding box. Defaults to the
  selected window.

  :save-to - a file to save the transformed image to (not
  implemented yet)

Examples:

  (image-transform img :resize 'fit-width)
  (image-transform img :resize 'fit-height)
  (image-transform img :resize 'fit)
  (image-transform img :resize 'fit :backend 'convert)
  (image-transform img :resize 'fit :rotate 45)
  (image-transform img :resize 'fit-height :rotate 60)

  (image-transform img :resize 200)
  (image-transform img :resize '(500 . 500))
  (image-transform img :resize '(500 500 \"!\"))

  (image-transform img :background \"pink\")
  (image-transform img :background \"pink\" :flatten t :backend 'convert)"
  (cond
   ;; image
   ((and (listp image) (eq (car image) 'image))
    (let* ((force-backend (plist-get specs :backend))
	   (backends (if (null force-backend)
			 (copy-sequence image-transform-backends)
		       (setq specs (image-tr--delete-properties specs '(:backend)))
		       (list force-backend)))
	   (bknd-errors "")
	   out bknd bknd-specs)

      (image-tr--osize image) ;; side effect of caching :osize
      ;; adjust for 'fit 'fit-width etc
      (setq specs (image-tr--adjust-specs-for-fit image specs))
      
      (while (and (null out)
		  (setq bknd (pop backends)))
	
	(let ((bknd-fun (intern (concat "image-transform:"
					(symbol-name bknd)))))
	  (condition-case data
	      (progn 
		(image-tr--check-unsupported-features image specs bknd)
		(setq out (apply bknd-fun image (cl-copy-list specs))))
	    (next-backend
	     (setq bknd-errors (format "%s\n%s: %s"
				       bknd-errors bknd (cadr data)))))))
      (unless out
	(error "All backends failed with the following errors: %s" bknd-errors))
      out))
   ;; list of images
   ((listp image)
    (dolist (i image) (apply #'image-transform i specs)))
   (t (error "Invalid image object"))))

(defun image-transform-interactive (&optional image &rest specs)
  "Like `image-transform' but also refresh window display.
Intended to be used for user level commands.  IMAGE defaults to
`image-at-point'.  SPECS is as in `image-transform'."
  (let ((image (or image (image-at-point))))
    (unless image
      (error "No image at point"))
    (prog1 (apply 'image-transform image specs)
      (force-window-update (selected-window)))))


;;;; Native Backend

(defvar image-transform-features:native
  '(("Image Settings"
     (:background "Background for images with transparent background")
     (:flatten "Does nothing. It's here for compatibility with 'convert' backend only." boolean))
    ("Geometry"
     (:resize "Resize the image" geometry)
     (:rotate "Rotation in degrees" degrees)
     (:scale "Scale in percent. Can be a number or numeric string." scale)))
  "Alist of supported features by 'native' backend.
Each element is a list of the form (FEATURE DOC READER-TYPE).
See `image-transform-backends' for a full description.")

(defun image-transform:native (image &rest specs)
  "Emacs's native ImageMagick transform backend.
IMAGE and SPECS are as in `image-transform'.

See `image-transform-features:native' for transform
specifications accepted by this backend."

  (unless (image-type-available-p 'imagemagick)
    (signal 'next-backend '("Emacs wasn't built with ImageMagick support")))
  
  (let* ((newtrs (image-tr--normalize-specs specs 'native))
         (trlist (image-tr--add-transforms
                  (image-get-transforms image) newtrs)))

    (image-put image :transforms trlist)
    
    ;; reset imagemagick properties 
    (image-tr--delete-properties image '(:width :height :rotation :background))

    ;; reset convert specs if any
    (when (image-get image :ofile)
      (image-tr--delete-properties (cdr image) '(:data))
      (image-put image :file (image-get image :ofile)))
    
    ;; now apply the transforms
    (cl-loop for s on (cdr trlist) by 'cddr do
             (when (image-tr--get-feature (car s) image-transform-features:native)
               (pcase (cons (car s) (cadr s))
                 (`(:resize . ,resize)
                  (pcase (image-transform-parse:geometry resize)
                    (`(,scale nil "%") (image-tr--rescale image scale))
                    (`(,sW ,sH "%") (image-tr--rescale image sW sH))
                    (`(,_area "@") (error "Area geometry is not implemented yet"))
                    (`(,W nil nil)
                     (image-put image :width W)
                     (image-tr--delete-properties image '(:height)))
                    (`(nil ,H nil)
                     (image-put image :height H)
                     (image-tr--delete-properties image '(:width)))
                    (`(,W ,H nil)
                     ;; Maximum values of height and width given, aspect ratio preserved.
                     (let ((osize (image-tr--osize image)))
                       (if (> (/ (float (car osize)) (cdr osize))
                              (/ (float W) H))
                           (progn (image-put image :width W)
                                  (image-tr--delete-properties image '(:height)))
                         (image-put image :height H)
                         (image-tr--delete-properties image '(:width)))))
                    (`(,W ,H "^")
                     ;; Minimum values of width and height given, aspect ratio
                     ;; preserved.
                     (let ((osize (image-tr--osize image)))
                       (if (< (/ (float (car osize)) (cdr osize))
                              (/ (float W) H))
                           (progn (image-put image :width W)
                                  (image-tr--delete-properties image '(:height)))
                         (image-put image :height H)
                         (image-tr--delete-properties image '(:width)))))
                    (`(,_W ,_H ">") (error "> operator is not implemented yet"))
                    (`(,_W ,_H "<") (error "< operator is not implemented yet"))
                    (`(,W ,H "!")
                     (if W
                         (image-put image :width W)
                       (image-tr--delete-properties image '(:width)))
                     (if H
                         (image-put image :height H)
                       (image-tr--delete-properties image '(:height))))
                    (`(,_W ,_H ,x)
                     ;; This should not happen, validity checks are
                     ;; in the geometry reader.
                     (error "Invalid geometry operator %s", x))))
                 (`(:rotate . ,rot)
                  (image-put image :rotation
                             (mod (+ (or (image-get image :rotation) 0.0)
                                     (image-transform-parse:number rot))
                                  360)))
                 (`(:scale . ,scale)
                  (image-tr--rescale image (image-transform-parse:scale scale)))
                 (`(:background . ,val)
                  (image-put image :background val))
		 (`(:flatten . ,_val))
                 ;; should never reach this place
                 (_ (error "Incorrect specification in 'native' backend")))))

    (image-put image :transform-backend 'native)
    (image-put image :type 'imagemagick)
    image))


;;;; Convert Backend

(defvar image-transform-features:convert 
  '(("Image Settings"
     (:antialias "remove pixel-aliasing" boolean)
     (:background "background color" color)
     (:bordercolor "border color" color)
     (:caption "annotate image with a caption" string)
     (:colors "preferred number of colors in the image" number)
     (:colorspace "alternate image colorspace"
		  (choice CMY CMYK Gray HCL HCLp HSB HSI HSL HSV
			  HWB Lab LCHab LCHuv LMS Log Luv OHTA
			  Rec601YCbCr Rec709YCbCr RGB scRGB sRGB
			  Transparent XYZ YCbCr YCC YDbDr YIQ
			  YPbPr YUV))
     (:comment "annotate image with comment" string)
     (:density "horizontal and vertical density of the image" geometry)
     (:depth "image depth" value)
     (:fill "color to use when filling a graphic primitive" color)
     (:filter "use this filter when resizing an image"
	      (choice Point Hermite Cubic Box Gaussian Catrom
		      Triangle Quadratic Mitchell))
     (:flatten "flatten a sequence of images" boolean)
     (:fuzz "colors within this distance are considered equal" distance)
     (:interlace "type of image interlacing scheme"
		 (choice none line plane partition JPEG GIF PNG))
     (:interpolate "pixel color interpolation method"
		   (choice integer nearest-neighbor average
			   bilinear mesh bicubic spline filter ))
     (:quality "JPEG/MIFF/PNG compression level" value)
     (:sampling-factor "horizontal and vertical sampling factor" geometry)
     (:statistic	"replace each pixel with corresponding statistic from the neighborhood"
			(choice Gradient Maximum Minimum Mean Median Mode Nonpeak))
     (:texture "name of texture to tile onto the image background" filename)
     (:tile-offset "tile offset" geometry))

    ("Text Settings"
     (:encoding "text encoding type"
		(choice AdobeCustom AdobeExpert AdobeStandard
			AppleRoman BIG5 GB2312 "Latin 2"
			None SJIScode Symbol Unicode Wansung))
     (:family "render text with this font family" name)
     (:font "render text with this font" name)
     (:gravity "horizontal and vertical text placement"
	       (choice NorthWest North NorthEast West
		       Center East SouthWest South SouthEast))
     (:pointsize "font point size" value)
     (:stretch "render text with this font stretch"
	       (choice Any Condensed Expanded ExtraCondensed
		       ExtraExpanded Normal SemiCondensed
		       SemiExpanded UltraCondensed UltraExpanded))
     (:style "render text with this font style"
	     (choice Any Italic Normal Oblique))
     (:stroke "graphic primitive stroke color" color)
     (:strokewidth "graphic primitive stroke width" value)
     (:weight "render text with this font weight"
	      (choice Undefined PixelsPerInch PixelsPerCentimeter)))

    ("Annotation"
     (:annotate "annotate the image with text" geometry-text ) ;; fixme
     (:draw "annotate the image with a graphic primitive" string))

    ("Geometry"
     (:adaptive-resize "adaptively resize image with data dependent triangulation" geometry)
     (:chop "remove pixels from the image interior" geometry)
     (:border "surround image with a border of color :bordercolor" geometry)
     (:extent "set the image size" geometry)
     (:extract "extract area from image" geometry)
     (:flip "flip image vertically" boolean)
     (:flop "flop image horizontally" boolean)
     (:resize "resize the image" geometry)
     (:roll "roll an image vertically or horizontally" geometry)
     (:rotate "apply Paeth rotation to the image" degrees)
     (:scale "scale the image" scale)
     (:shave "shave pixels from the image edges" geometry)
     (:shear "slide one edge of the image along the X or Y axis" geometry)
     (:thumbnail "create a thumbnail (aka fast resize)" geometry)
     (:transpose "flip vertically and rotate 90 degrees" boolean)
     (:transverse "flop horizontally and rotate 270 degrees" boolean)
     (:trim "trim edges" boolean))

    ("Effects"
     (:adaptive-blur "adaptively blur pixels, decrease effect near edges" geometry)
     (:adaptive-sharpen "adaptively sharpen pixels, increase effect near edges" geometry)
     (:black-threshold "force all pixels below the threshold into black" value)
     (:blur "reduce image noise and reduce detail levels" geometry)
     (:charcoal "simulate a charcoal drawing" radius)
     (:colorize "colorize the image with the :fill color" value)
     (:contrast- "reduce the image contrast" boolean)
     (:contrast+ "enhance the image contrast" boolean)
     (:contrast-stretch "improve contrast by `stretching the intensity range" geometry)
     (:cycle "cycle the image colormap" amount)
     (:despeckle "reduce the speckles within an image" boolean)
     (:edge "apply a filter to detect edges in the image" radius)
     (:emboss "emboss an image" radius)
     (:enhance "apply a digital filter to enhance a noisy image" boolean)
     (:equalize "perform histogram equalization to an image" boolean)
     (:gamma "level of gamma correction" value)
     (:gaussian-blur "reduce image noise and reduce detail levels" geometry)
     (:implode "implode image pixels about the center" amount)
     (:lat "local adaptive thresholding" geometry)
     (:level "adjust the level of image contrast" value)
     (:linear-stretch "improve contrast by `stretching with saturation the intensity range" geometry)
     (:median "apply a median filter to the image" geometry)
     (:mode "make each pixel the predominant color of the neighborhood" geometry)
     (:modulate "vary the brightness, saturation, and hue" value)
     (:monochrome "transform image to black and white" boolean)
     (:motion-blur "simulate motion blur" geometry)
     (:negate "replace each pixel with its complementary color" boolean)
     (:noise- "reduce noise in an image" radius)
     (:noise+ "add noise to an image"
	      (choice Gaussian Impulse Laplacian Multiplicative Poisson Random Uniform))
     (:normalize "transform image to span the full range of colors" boolean)
     (:opaque "change this color to the :fill color" color)
     (:paint "simulate an oil painting" radius)
     (:polaroid "simulate a Polaroid picture" angle)
     (:+polaroid "Polaroid picture with random rotation angle between -15 and 15 degrees" boolean)
     (:posterize "reduce the image to a limited number of color levels" levels)
     (:radial-blur "radial blur the image" angle)
     (:raise "darken image edges to create a 3-D effect" value)
     (:+raise "lighten image edges to create a 3-D effect" value)
     (:resample "change the resolution of an image" geometry)
     (:sample "scale image with pixel sampling" geometry)
     (:selective-blur "selectively blur pixels within a contrast threshold" geometry)
     (:sepia-tone "simulate a sepia-toned photo" threshold)
     (:shade "shade the image using a distant light source" degrees)
     (:shadow "simulate an image shadow" geometry)
     (:sharpen "sharpen the image" geometry)
     (:sigmoidal-contrast "lightness rescaling using sigmoidal contrast enhancement" geometry)
     (:sketch "simulate a pencil sketch" geometry)
     (:solarize "negate all pixels above the threshold level" threshold)
     (:splice "splice the background color into the image" geometry)
     (:swirl "swirl image pixels about the center" degrees)
     (:threshold "simultaneous black/white threshold" value)
     (:tile "tile image when filling a graphic primitive" filename)
     (:tint "tint the image with the :fill color" value)
     (:transparent "make this color transparent" color)
     (:unsharp "un-sharpen the image" geometry)
     (:vignette "soften the edges of the image in vignette style" geometry)
     (:wave "alter an image along a sine wave" geometry)
     (:white-threshold "force all pixels above the threshold into white" value)
     ))
  "Alist of supported features by \"convert\" backend.
Each element is a list of the form (FEATURE DOC READ-TYPE). See
`image-transform-backends' for a complete description.

Most of the arguments are passed to 'convert' program directly,
but some are reprocessed to overcome various limitations of the
convert command line tool. For example :contrast+ and :contrast-
are transformed into -contrast and +contrast respectively,
':caption' meta data is inserted as -set caption. See the code of
`image-tr--preprocess-convert-arg' for all pre-processing steps.

Use `image-transform-describe-convert-option' for online
documentation of a specific option, or visit:

   http://www.imagemagick.org/Usage/transform
   http://www.imagemagick.org/script/command-line-processing.php")

(defun image-tr--preprocess-convert-arg (arg &optional name-only)
  "Make ARG compatible with with convert specification.
For most args this means replacing leading ':' with '-'. If
optional NAME-ONLY is non-nil, don't perform complex
transformation of arguments (like :caption -> -set caption)."
  (cond ((and
	  (not name-only)
	  (member arg '(":caption" ":comment" ":label")))
	 (list "-set" (substring arg 1)))
	((equal arg ":contrast-") '("+contrast"))
	((equal arg ":contrast+") '("-contrast"))
	((equal arg ":noise-") '("-noise"))
	((equal arg ":noise+") '("+noise"))
	((string-match-p "^:\\+" arg)
	 (list (substring arg 1)))
	(t (list (concat "-" (substring arg 1))))))

(defun image-tr--convert-args (&optional image concat)
  "Build from IMAGE a list of arguments suitable for `call-process'.
If CONCAT is non-nil, also concatenate arguments and return a
string instead of a list."
  (setq image (or image (image-at-point)))
  (let* ((transforms (cdr (image-get image :transforms)))
         (args
	  (delq nil 
		(cl-loop for s on transforms by 'cddr
			 append
			 (let ((type (nth 2 (image-tr--get-feature
					     (car s) image-transform-features:convert)))
			       (opt (symbol-name (car s))))
			   (append (image-tr--preprocess-convert-arg opt)
				   (list (unless (eq 'boolean type)
					   (format "%s" (cadr s))))))))))
    (if concat
        (mapconcat 'identity args " ")
      args)))

(defun image-tr--executable-find (program)
  (or (executable-find program)
      (signal 'next-backend
	      (list (format "Cannot find executable '%s'" program)))))

(defun image-tr--process-convert-transforms (image)
  "Call \"convert\" on IMAGE and insert transformed data as :data image spec.
IMAGE is modified destructively. Currently, only images loaded
from a file can be handled by this backend."
  (let* ((convert-prog (image-tr--executable-find image-transform-convert-program))
	 (specs (cdr image))
         (buf (get-buffer-create "*image-tr-output*"))
	 (file (plist-get specs :file))
         (ofile (or (plist-get specs :ofile)
		    file
		    (signal 'next-backend
			    (list "Image is not associated with a file"))))
         (type (plist-get specs :type))
         (log-file (make-temp-file "image-transform.log")) ; debug only
         (args (append (image-tr--convert-args image) '("-"))))

    (when (or (eq type 'imagemagick)
              (null type))
      (setq type
            (if file
                (image-type ofile)
              (image-type (image-get image :data) nil t))))
    
    (plist-put specs :ofile ofile)

    (when image-transform-verbose
      (message "command: %s %s %s"
	       image-transform-convert-program
	       (expand-file-name ofile)
	       (combine-and-quote-strings args)))

    (plist-put specs :data
               (with-current-buffer buf
                 (erase-buffer)
                 (set-buffer-multibyte nil)
                 (when (/= 0 (apply 'process-file convert-prog
                                    nil (list t log-file) nil (expand-file-name ofile) args))
                   (erase-buffer)
		   (insert (format "command: %s %s %s\n\n"
				   image-transform-convert-program
				   (expand-file-name ofile)
				   (combine-and-quote-strings args)))
                   (insert-file-contents log-file)
		   (delete-file log-file)
                   (signal 'next-backend
			   (list (format "convert error: %s" (buffer-string)))))
                 (buffer-string)))

    (delete-file log-file)
    (image-tr--delete-properties image '(:file))
    (plist-put specs :type type)
    
    image))

(defun image-transform:convert (image &rest specs)
  "ImageMagick Convert backend.
Convert IMAGE through a call to \"convert\" program and pass
transforms in SPECS.  SPECS are as in `image-transform'. See
`image-transform-features:convert' for transforms accepted by
this backend."
  (let* ((image-transform-accept-unparsed t) ; pass directly to convert
         (newtrs (image-tr--normalize-specs specs 'convert))
         (trlist (image-tr--add-transforms
                  (image-get-transforms image) newtrs)))
    (image-put image :transforms trlist)
    (image-put image :transform-backend 'convert)
    (image-tr--process-convert-transforms image)))

(defvar image-tr--describe-hist nil)
(defun image-transform-describe-convert-option (&optional option)
  "Display an online documentation of \"convert\" OPTION."
  (interactive)
  (let* ((opts (mapcar (lambda (x)
			 (substring (symbol-name x) 1))
		       (image-tr--available-features 'convert)))
         (opt (or option
		  (completing-read "Convert option: " (append '("*ALL*") opts)
				   nil t nil 'image-tr--describe-hist))))
    (browse-url (if (equal opt "*ALL*")
                    "http://www.imagemagick.org/script/convert.php"
                  (format "http://www.imagemagick.org/script/command-line-options.php#%s"
			  (substring (car (image-tr--preprocess-convert-arg (concat ":" opt) t)) 1))))))


;;; Transform submenus

(defun image-tr--current-transforms-menu (menu)
  "Generate submenu for current transforms"
  (let ((trs (cdr (image-get-transforms))))
    (cl-loop for tr on trs by #'cddr
	     for i = 1 then (1+ i)
	     collect `[,(format "%s %s" (car tr) (cadr tr))
		       (lambda () (interactive) (image-transform-modify nil ,i))
		       :help "Modify this transform"])))

(defun image-tr--all-transforms-menu (menu)
  "Generate a submenu of all transforms."
  (let (all)
    (dolist (bknd image-transform-backends)
      (let ((features (symbol-value (intern (format "image-transform-features:%s" bknd)))))
	(dolist (fs features)
	  (setq all (lax-plist-put all (car fs) (append (lax-plist-get all (car fs))
							(cdr fs)))))))
    (append
     '(["Add" image-transform-add :help "Add Transforms Interactively"]
       ["Delete" image-transform-delete]
       ["Modify" image-transform-modify]
       ["List" image-transform-list]
       "--")
     (sort (cl-loop for gr on all by #'cddr
		    collect (cons (car gr)
				  (mapcar (lambda (el)
					    `[,(symbol-name (car el))
					      (lambda () (interactive) (image-transform-add nil ,(car el)))
					      :help ,(cadr el)])
					  (cl-delete-duplicates (delq nil (cadr gr))
								:test (lambda (a b) (eq (car a) (car b)))))))
	   (lambda (a b) (string< (car a) (car b)))))))

;; (image-tr--transforms-menu nil)


;;; Transform UI

(defvar image-tr--add-transform-hist nil)

(defun image-tr--read-transform (tr)
  "Interactively ask for value for the transform TR."
  (let* ((type (nth 2 (cl-loop for bknd in image-transform-backends
			       for val = (image-tr--get-feature tr bknd)
			       if val return val)))
	 (prompt (format "%s (%s): " tr type)))
    (pcase type
      (`boolean t)
      (`(choice . ,opts) (completing-read prompt opts))
      (`number (read-number prompt))
      (`filename (read-file-name prompt))
      (_ (read-string prompt)))))

(defun image-transform-add (&optional image transform)
  "Interactively add transform to IMAGE.
IMAGE defaults to `image-at-point'.  TRANSFORM is a keyword
naming the transformation. Don't use this function in programs,
use `image-transform' instead."
  (interactive)
  (let* ((allopts (delete-dups (cl-mapcan #'image-tr--available-features image-transform-backends)))
         (tr (if transform
		 (symbol-name transform)
	       (completing-read "Transform: " allopts nil t nil 'image-tr--add-transform-hist)))
         (tr-symb (intern tr))
	 (value (image-tr--read-transform tr-symb)))
    (image-transform-interactive image tr-symb value)))

(defun image-transform-list (&optional image)
  "Print all transforms associated with IMAGE.
IMAGE defaults to `image-at-point'."
  (interactive)
  (message "%s" (cdr (image-get-transforms image))))

(defun image-transform-delete (&optional image transform)
  "Remove from IMAGE the TRANSFORM.
IMAGE defaults to `image-at-point'.  Don't use this function in
programs, use `image-tr--delete-transforms' instead."
  (interactive)
  (setq image (or image (image-at-point)))
  (let* ((trs (cl-loop for el in (image-get-transforms image)
		       if (keywordp el) collect (symbol-name el)))
         (tr (or transform
		 (if trs
		     (intern (completing-read "Delete transform: "
					      (append '("*ALL*") trs) nil t))
		   (error "No transforms for current image")))))
    (if (equal tr '*ALL*)
	(image-put image :transforms '(tr))
      (image-tr--delete-transforms image (list tr)))
    (image-transform-interactive image)))

(defun image-transform-modify (&optional image N)
  "Modify IMAGE's Nth transform.
When called interactively ask for a transform. IMAGE defaults to
`image-at-point'."
  (interactive)
  (setq image (or image (image-at-point)))
  (let* ((ixs)
	 (counter 0)
	 (trs (cdr (image-get-transforms image)))
	 (names (and (not N)
		     (cl-loop for el in trs by #'cddr
			      collect (let ((ix (1+ (or (plist-get ixs el) 0))))
					(setq ixs (plist-put ixs el ix)
					      counter (1+ counter))
					(propertize
					 (if (= ix 1)
					     (symbol-name el)
					   (format "%s<%d>" el ix))
					 :tr-N counter)))))
         (N (or N
		(if names
		    (get-text-property 0 :tr-N
				       (completing-read "Modify transform: " names nil t))
		  (error "No transforms for current image"))))
	 (tr (nth (- (* 2 N) 2) trs))
	 (value (image-tr--read-transform tr)))
    (image-tr--modify-transform image N value)
    (image-transform-interactive image)))

(provide 'image-transform)
;;; image-transform.el ends here
