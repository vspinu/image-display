;;; image-mode.el --- support for visiting image files  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2005-2013 Free Software Foundation, Inc.
;;
;; Author: Richard Stallman <rms@gnu.org>
;; Keywords: multimedia
;; Package: emacs

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Defines a major mode for visiting image files
;; that allows conversion between viewing the text of the file
;; and viewing the file as an image.  Viewing the image
;; works by putting a `display' text-property on the
;; image data, with the image-data still present underneath; if the
;; resulting buffer file is saved to another name it will correctly save
;; the image data to the new file.

;; Todo:

;; Consolidate with doc-view to make them work on directories of images or on
;; image files containing various "pages".

;;; Code:

(require 'image)
(require 'image-transform)
;; (eval-when-compile (require 'cl-lib))

(defgroup image-mode ()
  "Support for visiting image files."
  :group 'multimedia)

(defcustom image-mode-auto-resize 'fit-if-large
  "The image resize default.

Can be:
 - a number, giving a proportional scaling of the image.
 - a cons, giving the actual size (w x h) in pixels.
 - a symbol:
   *`fit' - maximally scale IMAGE to fit into window
   *`fit-if-large' - like `fit', but only when image is larger than window
   *`fit-height' - fit the image to window height
   *`fit-width' - fit the image to window width
   *`fit-stretch' - stretch the image to fit to both height and
    width of the window"
  :type '(choice
          (const :tag "no resize" nil)
          (number :tag "scale")
          (cons :tag "size (w . h)" number number)
          (const :tag "fit" fit)
          (const :tag "fit if large" fit-if-large)
          (const :tag "fit height" fit-height)
          (const :tag "fit width" fit-width)
          (const :tag "fit stretch" fit-stretch))
  :group 'image-mode
  :version "24.4")

;; This one is not customizable
(defvar image-mode-auto-rotate nil
  "Default rotation angle for the image.
Nil means no rotation.")

(defcustom image-mode-show-cursor t
  "Non-nil if the cursor should be shown in image-mode"
  :group 'image-mode
  :type 'boolean)

;;; Image mode window-info management.

(defvar-local image-mode-winprops-alist t)

(defvar image-mode-new-window-functions nil
  "Special hook run when image data is requested in a new window.
It is called with one argument, the initial WINPROPS.")

(defun image-mode-winprops (&optional window cleanup)
  "Return winprops of WINDOW.
A winprops object has the shape (WINDOW . ALIST).
WINDOW defaults to `selected-window' if it displays the current buffer, and
otherwise it defaults to t, used for times when the buffer is not displayed."
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
      (when (image-get-display-property) ;Only do it if we display an image!
        (if hscroll (set-window-hscroll (selected-window) hscroll))
        (if vscroll (set-window-vscroll (selected-window) vscroll))))))

(defun image-mode-setup-winprops ()
  ;; Record current scroll settings.
  (unless (listp image-mode-winprops-alist)
    (setq image-mode-winprops-alist nil))
  (add-hook 'window-configuration-change-hook
            'image-mode-reapply-winprops nil t))

;;; Image scrolling functions


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

(defun image-forward-hscroll (&optional n)
  "Scroll image in current window to the left by N character widths.
Stop if the right edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
        ((< n 0)
         (image-set-window-hscroll (max 0 (+ (window-hscroll) n))))
        (t
         (let* ((image (image-get-display-property))
                (edges (window-inside-edges))
                (win-width (- (nth 2 edges) (nth 0 edges)))
                (img-width (ceiling (car (image-display-size image)))))
           (image-set-window-hscroll (min (max 0 (- img-width win-width))
                                          (+ n (window-hscroll))))))))

(defun image-backward-hscroll (&optional n)
  "Scroll image in current window to the right by N character widths.
Stop if the left edge of the image is reached."
  (interactive "p")
  (image-forward-hscroll (- n)))

(defun image-next-line (n)
  "Scroll image in current window upward by N lines.
Stop if the bottom edge of the image is reached."
  (interactive "p")
  (cond ((= n 0) nil)
        ((< n 0)
         (image-set-window-vscroll (max 0 (+ (window-vscroll) n))))
        (t
         (let* ((image (image-get-display-property))
                (edges (window-inside-edges))
                (win-height (- (nth 3 edges) (nth 1 edges)))
                (img-height (ceiling (cdr (image-display-size image)))))
           (image-set-window-vscroll (min (max 0 (- img-height win-height))
                                          (+ n (window-vscroll))))))))

(defun image-previous-line (&optional n)
  "Scroll image in current window downward by N lines.
Stop if the top edge of the image is reached."
  (interactive "p")
  (image-next-line (- n)))

(defun image-scroll-up (&optional n)
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
           (image-next-line
            (max 0 (- win-height next-screen-context-lines)))))
        ((eq n '-)
         (let* ((edges (window-inside-edges))
                (win-height (- (nth 3 edges) (nth 1 edges))))
           (image-next-line
            (min 0 (- next-screen-context-lines win-height)))))
        (t (image-next-line (prefix-numeric-value n)))))

(defun image-scroll-down (&optional n)
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
           (image-next-line
            (min 0 (- next-screen-context-lines win-height)))))
        ((eq n '-)
         (let* ((edges (window-inside-edges))
                (win-height (- (nth 3 edges) (nth 1 edges))))
           (image-next-line
            (max 0 (- win-height next-screen-context-lines)))))
        (t (image-next-line (- (prefix-numeric-value n))))))

(defun image-bol (arg)
  "Scroll horizontally to the left edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-next-line (- arg 1)))
  (image-set-window-hscroll 0))

(defun image-eol (arg)
  "Scroll horizontally to the right edge of the image in the current window.
With argument ARG not nil or 1, move forward ARG - 1 lines first,
stopping if the top or bottom edge of the image is reached."
  (interactive "p")
  (and arg
       (/= (setq arg (prefix-numeric-value arg)) 1)
       (image-next-line (- arg 1)))
  (let* ((image (image-get-display-property))
         (edges (window-inside-edges))
         (win-width (- (nth 2 edges) (nth 0 edges)))
         (img-width (ceiling (car (image-display-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))))

(defun image-bob ()
  "Scroll to the top-left corner of the image in the current window."
  (interactive)
  (image-set-window-hscroll 0)
  (image-set-window-vscroll 0))

(defun image-eob ()
  "Scroll to the bottom-right corner of the image in the current window."
  (interactive)
  (let* ((image (image-get-display-property))
         (edges (window-inside-edges))
         (win-width (- (nth 2 edges) (nth 0 edges)))
         (img-width (ceiling (car (image-display-size image))))
         (win-height (- (nth 3 edges) (nth 1 edges)))
         (img-height (ceiling (cdr (image-display-size image)))))
    (image-set-window-hscroll (max 0 (- img-width win-width)))
    (image-set-window-vscroll (max 0 (- img-height win-height)))))

;; Adjust frame and image size.

(defun image-mode-fit-frame (&optional frame toggle)
  "Fit FRAME to the current image.
If FRAME is omitted or nil, it defaults to the selected frame.
All other windows on the frame are deleted.

If called interactively, or if TOGGLE is non-nil, toggle between
fitting FRAME to the current image and restoring the size and
window configuration prior to the last `image-mode-fit-frame'
call."
  (interactive (list nil t))
  (let* ((buffer (current-buffer))
         (display (image-get-display-property))
         (size (image-display-size display))
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

;;; Image Mode setup

(defvar-local image-type nil
  "The image type for the current Image mode buffer.")

(defvar-local image-multi-frame nil
  "Non-nil if image for the current Image mode buffer has multiple frames.")

(defvar image-mode-previous-major-mode nil
  "Internal variable to keep the previous non-image major mode.")

(defvar image-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "\C-c\C-c" 'image-toggle-display)
    (define-key map (kbd "SPC")       'image-scroll-up)
    (define-key map (kbd "S-SPC")     'image-scroll-down)
    (define-key map (kbd "DEL")       'image-scroll-down)
    (define-key map (kbd "RET")       'image-toggle-animation)
    (define-key map "T" 'image-mode-show-thumbnails)
    (define-key map "F" 'image-goto-frame)
    (define-key map "f" 'image-next-frame)
    (define-key map "b" 'image-previous-frame)
    (define-key map "n" 'image-next-file)
    (define-key map "p" 'image-previous-file)
    (define-key map [remap forward-char] 'image-forward-hscroll)
    (define-key map [remap backward-char] 'image-backward-hscroll)
    (define-key map [remap right-char] 'image-forward-hscroll)
    (define-key map [remap left-char] 'image-backward-hscroll)
    (define-key map [remap previous-line] 'image-previous-line)
    (define-key map [remap next-line] 'image-next-line)
    (define-key map [remap scroll-up] 'image-scroll-up)
    (define-key map [remap scroll-down] 'image-scroll-down)
    (define-key map [remap scroll-up-command] 'image-scroll-up)
    (define-key map [remap scroll-down-command] 'image-scroll-down)
    (define-key map [remap move-beginning-of-line] 'image-bol)
    (define-key map [remap move-end-of-line] 'image-eol)
    (define-key map [remap beginning-of-buffer] 'image-bob)
    (define-key map [remap end-of-buffer] 'image-eob)
    (easy-menu-define image-mode-menu map "Menu for Image mode."
      '("Image"
        ["Show as Text" image-toggle-display
         :active t
         :help "Show image as text"]
        "--"
        ["Fit Frame to Image" image-mode-fit-frame
         :active t
         :help "Resize frame to match image"]
        ["Fit into Window" image-scale-to-fit-window
         :visible (eq image-type 'imagemagick)
         :help "Maximally resize image to fit into window"]
        ["Fit to Window Height" image-scale-to-fit-height
         :visible (eq image-type 'imagemagick)
         :help "Resize image to match the window height"]
        ["Fit to Window Width" image-scale-to-fit-width
         :visible (eq image-type 'imagemagick)
         :help "Resize image to match the window width"]
        ["Rotate Image..." image-rotate
         :visible (eq image-type 'imagemagick)]
        ["Rotate Image Right" image-rotate-right
         :visible (eq image-type 'imagemagick)]
        ["Rotate Image Left" image-rotate-left
         :visible (eq image-type 'imagemagick)]
        ["Change Image Background..." image-change-background
         :visible (eq image-type 'imagemagick)]
        "--"
        ["Show Thumbnails" image-mode-show-thumbnails
         :active default-directory
         :help "Show thumbnails for all images in this directory"]
        ["Next Image" image-next-file :active buffer-file-name
         :help "Move to next image in this directory"]
        ["Previous Image" image-previous-file :active buffer-file-name
         :help "Move to previous image in this directory"]
        "--"
        ["Animate Image" image-toggle-animation :style toggle
         :selected (let ((image (get-image)))
                     (and image (image-animate-timer image)))
         :active image-multi-frame
         :help "Toggle image animation"]
        ["Loop Animation"
         (lambda () (interactive)
           (setq image-animate-loop (not image-animate-loop))
           ;; FIXME this is a hacky way to make it affect a currently
           ;; animating image.
           (when (let ((image (image-get-display-property)))
                   (and image (image-animate-timer image)))
             (image-toggle-animation)
             (image-toggle-animation)))
         :style toggle :selected image-animate-loop
         :active image-multi-frame
         :help "Animate images once, or forever?"]
        ["Next Frame" image-next-frame :active image-multi-frame
         :help "Show the next frame of this image"]
        ["Previous Frame" image-previous-frame :active image-multi-frame
         :help "Show the previous frame of this image"]
        ["Goto Frame..." image-goto-frame :active image-multi-frame
         :help "Show a specific frame of this image"]
        ))
    map)
  "Mode keymap for `image-mode'.")

(image--add-transform-keys image-mode-map)

(defvar image-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'image-toggle-display)
    map)
  "Mode keymap for `image-minor-mode'.")

(defvar bookmark-make-record-function)

(put 'image-mode 'mode-class 'special)

;;;###autoload
(defun image-mode ()
  "Major mode for image files.
You can use \\<image-mode-map>\\[image-toggle-display]
to toggle between display as an image and display as text.

\\{image-mode-map\}"
  (interactive)
  (condition-case err
      (progn
        (unless (display-images-p)
          (error "Display does not support images"))

        (kill-all-local-variables)
        (setq major-mode 'image-mode)

        (if (not (image-get-display-property))
            (progn
              (image-toggle-display-image)
              ;; If attempt to display the image fails.
              (if (not (image-get-display-property))
                  (error "Invalid image")))
          ;; Set next vars when image is already displayed but local
          ;; variables were cleared by kill-all-local-variables
          (setq cursor-type nil truncate-lines t
                image-type (plist-get (cdr (image-get-display-property)) :type)))

        (setq mode-name (if image-type (format "Image[%s]" image-type) "Image"))
        (use-local-map image-mode-map)

        ;; Use our own bookmarking function for images.
        (setq-local bookmark-make-record-function
                    #'image-bookmark-make-record)

        ;; Keep track of [vh]scroll when switching buffers
        (image-mode-setup-winprops)

        ;; fixme: should be rewritten whiteout actually re-installing
        ;; the mode, user vars are lost + deriving modes is difficult
        (set (make-local-variable 'revert-buffer-function)
             'image-mode-revert-buffer-function)

        (add-hook 'change-major-mode-hook 'image-toggle-display-text nil t)
        (add-hook 'after-revert-hook 'image-after-revert-hook nil t)
        (run-mode-hooks 'image-mode-hook)
        (let ((image (image-get-display-property))
              (msg1 (substitute-command-keys
                     "Type \\[image-toggle-display] to view the image as "))
              animated)
          (cond
           ((null image)
            (message "%s" (concat msg1 "an image.")))
           ((setq animated (image-multi-frame-p image))
            (setq image-multi-frame t
                  mode-line-process
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
;;;                          (substitute-command-keys
;;;                           "\\[image-toggle-animation] to animate."))))
           (t
            (message "%s" (concat msg1 "text."))))))

    (error
     (image-mode-as-text)
     (funcall
      (if (called-interactively-p 'any) 'error 'message)
      "Cannot display image: %s" (cdr err)))))

(defun image-mode-revert-buffer-function (ignore noconfirm)
  ;; don't ask on reversion
  (let ((revert-buffer-function nil))
    (revert-buffer ignore t)))

;;;###autoload
(define-minor-mode image-minor-mode
  "Toggle Image minor mode in this buffer.
With a prefix argument ARG, enable Image minor mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

Image minor mode provides the key \\<image-mode-map>\\[image-toggle-display],
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
\\<image-mode-map>\\[image-toggle-display] to switch back to `image-mode'
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
    (image-toggle-display-text)
    (message "%s" (concat
                   (substitute-command-keys
                    "Type \\[image-toggle-display] to view the image as ")
                   (if (image-get-display-property)
                       "text" "an image") "."))))

(define-obsolete-function-alias 'image-mode-maybe 'image-mode "23.2")

(defun image-toggle-display-text ()
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

(defun image-mode-show-thumbnails ()
  "Show thumbnails alongside dired buffer.
Based on `image-dired'"
  (interactive)
  (image-dired default-directory))

(defvar archive-superior-buffer)
(defvar tar-superior-buffer)
(declare-function image-flush "image.c" (spec &optional frame))

(defun image-toggle-display-image ()
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
    (setq image (image-transform-interactive image
                                             :resize image-mode-auto-resize
                                             :rotate image-mode-auto-rotate))
    (setq props
          `(display ,image
                    ;; intangible ,image
                    rear-nonsticky (display) ;; intangible
                    read-only t front-sticky (read-only)))

    (let ((buffer-file-truename nil)) ; avoid changing dir mtime by lock_file
      (add-text-properties (point-min) (point-max) props)
      (restore-buffer-modified-p modified))
    ;; Inhibit the cursor when the buffer contains only an image,
    ;; because cursors look very strange on top of images.

    ;; VS[16-07-2013]: It is a blinking box around image. Not a big
    ;; deal. It is way more important to distinguish active
    ;; buffer/image. In the future we will have multiple images per
    ;; buffer. Will need to activate it anyhow.

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
    ;; (image--transform-check-size)
    (if (called-interactively-p 'any)
        (message "Repeat this command to go back to displaying the file as text"))))

(defun image-toggle-display ()
  "Toggle between image and text display.
If the current buffer is displaying an image file as an image,
call `image-mode-as-text' to switch to text.  Otherwise, display
the image by calling `image-mode'."
  (interactive)
  (if (image-get-display-property)
      (image-mode-as-text)
    (image-mode)))

(defun image-after-revert-hook ()
  (when (image-get-display-property)
    (image-toggle-display-text)
    ;; Update image display.
    (mapc (lambda (window) (redraw-frame (window-frame window)))
          (get-buffer-window-list (current-buffer) 'nomini 'visible))
    (image-toggle-display-image)))


;;; Animated images

(defcustom image-animate-loop nil
  "Non-nil means animated images loop forever, rather than playing once."
  :type 'boolean
  :version "24.1"
  :group 'image)

(defun image-toggle-animation ()
  "Start or stop animating the current image.
If `image-animate-loop' is non-nil, animation loops forever.
Otherwise it plays once, then stops."
  (interactive)
  (let ((image (image-get-display-property))
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

(defun image-goto-frame (n &optional relative)
  "Show frame N of a multi-frame image.
Optional argument OFFSET non-nil means interpret N as relative to the
current frame.  Frames are indexed from 1."
  (interactive
   (list (or current-prefix-arg
             (read-number "Show frame number: "))))
  (let ((image (image-get-display-property)))
    (cond
     ((null image)
      (error "No image is present"))
     ((null image-multi-frame)
      (message "No image animation."))
     (t
      (image-show-frame image
                        (if relative
                            (+ n (image-current-frame image))
                          (1- n)))))))

(defun image-next-frame (&optional n)
  "Switch to the next frame of a multi-frame image.
With optional argument N, switch to the Nth frame after the current one.
If N is negative, switch to the Nth frame before the current one."
  (interactive "p")
  (image-goto-frame n t))

(defun image-previous-frame (&optional n)
  "Switch to the previous frame of a multi-frame image.
With optional argument N, switch to the Nth frame before the current one.
If N is negative, switch to the Nth frame after the current one."
  (interactive "p")
  (image-next-frame (- n)))


;;; Switching to the next/previous image

(defun image-next-file (&optional n)
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

(defun image-previous-file (&optional n)
  "Visit the preceding image in the same directory as the current file.
With optional argument N, visit the Nth image file preceding the
current one, in cyclic alphabetical order.

This command visits the specified file via `find-alternate-file',
replacing the current Image mode buffer."
  (interactive "p")
  (image-next-file (- n)))

(defun image-mode--images-in-directory (file)
  (let* ((dir (file-name-directory buffer-file-name))
         (files (directory-files dir nil
                                 (image-file-name-regexp) t)))
    ;; Add the current file to the list of images if necessary, in
    ;; case it does not match `image-file-name-regexp'.
    (unless (member file files)
      (push file files))
    (sort files 'string-lessp)))


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
      (image-toggle-display))))


(provide 'image-mode)

;;; image-mode.el ends here
