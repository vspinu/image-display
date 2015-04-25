
(require 'ert)
(require 'noflet)

(setq img (create-image (expand-file-name "img/test2.png")))
(setq butterfly (create-image (expand-file-name "img/butterfly.png")))
(setq voice (create-image (expand-file-name "img/voice.gif")))
(setq logo (create-image (expand-file-name "img/logo.png")))

(image-size butterfly t)


;;; Resize

(ert-deftest test-image-transform-resize:native ()
  (let ((image-transform-backends '(native))
	(img butterfly))
    (noflet ((window-inside-pixel-edges (&optional window) (list 0 0 100 200)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-width) t)
		     (cons 100 110)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-height) t)
		     (cons 182 200)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit) t)
		     (cons 100 110)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-stretch) t)
		     (cons 100 200)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-if-large) t)
		     (cons 100 110)))
      (should (equal (image-size (image-transform (copy-list img) :resize 300) t)
		     (cons 300 329)))
      (should (equal (image-size (image-transform (copy-list img) :resize '(300 600)) t)
		     (cons 300 329)))
      (should (equal (image-size (image-transform (copy-list img) :resize "300x600") t)
		     (cons 300 329)))
      (should (equal (image-size (image-transform (copy-list img) :resize '(300 600 "!")) t)
		     (cons 300 600)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "300x600!") t)
		     (cons 300 600)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "300x600^") t)
		     (cons 547 600)))
      ;; Not implemented yet:
      ;; (should (equal (image-size (image-transform (copy-list img) :resize  "300x600>") t)
      ;; 		     (cons 277 304)))
      ;; (should (equal (image-size (image-transform (copy-list img) :resize  "200x600>") t)
      ;; 		     (cons 200 219)))
      ;; (should (equal (image-size (image-transform (copy-list img) :resize  "300x600<") t)
      ;; 		     (cons 300 329)))
      ;; (should (equal (image-size (image-transform (copy-list img) :resize  "6000@") t)
      ;; 		     (cons 74 81)))
      )))

(ert-deftest test-image-transform-resize-box ()
  (let ((img butterfly))
    (should (equal (image-size (image-transform (copy-list img) :resize 'fit-height
						:box (cons 100 200))
			       t)
		   (cons 182 200)))
    (should (equal (image-size (image-transform (copy-list img) :resize 'fit
						:box (cons 100 200))
			       t)
		   (cons 100 110)))))

(ert-deftest test-image-transform-resize:convert ()
  (let ((image-transform-backends '(convert))
	(img butterfly))
    (noflet ((window-inside-pixel-edges (&optional window) (list 0 0 100 200)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-width) t)
		     (cons 100 110)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-height) t)
		     (cons 182 200)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit) t)
		     (cons 100 110)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-stretch) t)
		     (cons 100 200)))
      (should (equal (image-size (image-transform (copy-list img) :resize 'fit-if-large) t)
		     (cons 100 110)))
      (should (equal (image-size (image-transform (copy-list img) :resize 300) t)
		     (cons 300 329)))
      (should (equal (image-size (image-transform (copy-list img) :resize '(300 600)) t)
		     (cons 300 329)))
      (should (equal (image-size (image-transform (copy-list img) :resize "300x600") t)
		     (cons 300 329)))
      (should (equal (image-size (image-transform (copy-list img) :resize '(300 600 "!")) t)
		     (cons 300 600)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "300x600!") t)
		     (cons 300 600)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "300x600^") t)
		     (cons 547 600)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "300x600>") t)
      		     (cons 277 304)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "200x600>") t)
      		     (cons 200 219)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "300x600<") t)
      		     (cons 300 329)))
      (should (equal (image-size (image-transform (copy-list img) :resize  "6000@") t)
      		     (cons 74 81)))
      )))


;; Rotation

(ert-deftest test-image-transform-rotate:native ()
  (let ((image-transform-backends '(native))
	(img butterfly))
    (noflet ((window-inside-pixel-edges (&optional window) (list 0 0 100 200)))
      (should (equal (image-size (image-transform (copy-list img) :rotate 90) t)
		     (cons 304 277)))
      (should (equal (image-size (image-transform (copy-list img) :rotate 180) t)
		     (cons 277 304)))
      (should (equal (image-size (image-transform (copy-list img) :rotate 45) t)
		     (cons 413 412)))
      (should (equal (image-size (image-transform (copy-list img)
						  :resize 'fit-height
						  :rotate 45) t)
		     (cons 199 200)))
      (should (equal (image-size (image-transform (copy-list img)
						  :resize 'fit-width
						  :rotate 45) t)
		     (cons 100 100)))
      (should (equal (image-size (image-transform (copy-list img)
						  :resize 'fit
						  :rotate 45) t)
		     (cons 100 100)))
      )))

(ert-deftest test-image-transform-rotate:convert ()
  (let ((image-transform-backends '(convert))
	(img butterfly))
    (noflet ((window-inside-pixel-edges (&optional window) (list 0 0 100 200)))
      (should (equal (image-size (image-transform (copy-list img) :rotate 90) t)
		     (cons 304 277)))
      (should (equal (image-size (image-transform (copy-list img) :rotate 180) t)
		     (cons 277 304)))
      (should (equal (image-size (image-transform (copy-list img) :rotate 45) t)
		     (cons 413 412)))
      (should (equal (image-size (image-transform (copy-list img)
						  :resize 'fit-height
						  :rotate 45) t)
		     (cons 199 200)))
      (should (equal (image-size (image-transform (copy-list img)
						  :resize 'fit-width
						  :rotate 45) t)
		     (cons 100 100)))
      (should (equal (image-size (image-transform (copy-list img)
						  :resize 'fit
						  :rotate 45) t)
		     (cons 100 100)))
      )))


;; Exchange order of backend application
(ert-deftest test-image-transform-conver-native-exchange ()
  (noflet ((window-inside-pixel-edges (&optional window) (list 0 0 100 200)))
    (let* ((image-transform-backends '(convert))
	   (img butterfly)
	   (img-convert (image-transform (copy-list img) :resize 'fit-width :rotate 45))
	   (image-transform-backends '(native))
	   (img-native (image-transform (copy-list img-convert))))
      (should (equal (image-size img-convert t)
		     (image-size img-native t))))))

(ert-deftest test-image-transform-native-conver-exchange ()
  (noflet ((window-inside-pixel-edges (&optional window) (list 0 0 100 200)))
    (let* ((image-transform-backends '(native))
	   (img butterfly)
	   (img-convert (image-transform (copy-list img) :resize 'fit-width :rotate 45))
	   (image-transform-backends '(convert))
	   (img-native (image-transform (copy-list img-convert) :flatten)))
      (should (equal (image-size img-convert t)
		     (image-size img-native t))))))


 
;;; Manual Visual Tests

(defun ii (image &rest args)
  (insert-image (apply 'image-transform (copy-list image) args)))

(ii img :resize '(500 . 500))
(ii img :resize '(500 . 500) :backend 'convert)

(ii img :resize 200)
(ii img :resize 'fit-width)
(ii img :resize 'fit-height)
(ii img :resize 'fit)
(ii img :resize 'fit-stretch)
(ii img :resize 'fit-if-large)
(ii img :resize 'fit :rotate 45)
(ii img :resize 'fit-height :rotate 60)
(ii butterfly :background "pink" :flatten t :backend 'convert)
(ii img :background "pink")


;; map
(ii img :resize 'fit
	      nil nil nil image-manipulation-map)
;;; convert:
(setq image-transform-backends '(convert))
(insert-image (image-transform-copy butterfly))

(ii img :resize '(500 . 500))
(ii img :resize 200)
(ii img :resize 'fit-width)
(ii img :resize 'fit-height)
(ii img :resize 'fit)
(ii img :resize 'fit-stretch)
(ii img :resize 'fit-if-large)
(insert-image (image-transform (copy-list butterfly) :resize 'fit-if-large))
(ii img :background "transparent")
(ii img :resize 'fit-height :rotate 45)
(ii img :resize 'fit :rotate 45)

(ii img :blabla 33 :rotate 45)
(ii img :resize 'fit-height :rotate 60)
;; flatten is needed for the background to take effect
(insert-image (image-transform (copy-list butterfly) :background "pink" :flatten))
(insert-image (image-transform (copy-list butterfly) :resize 'fit-height
			       :background "pink" :flatten))



;;; EFFECTS

(ii img :caption "Caption: %c %f\n%wx%h" :gravity "Center" :background "black" :+polaroid t)

(ii img :thumbnail "220x220" :font "Candice" :pointsize 18 
    :bordercolor "Snow" :background "black" :fill "dodgerblue" :stroke "navy"
    :gravity "Center"  :caption "Caption!"  :polaroid 10)

(ii img :raise 15)
(ii butterfly :paint 5)
(ii butterfly :colorspace "Gray"  :edge 1 :negate t )

(ii voice :edge 1 :negate t)

(ii voice :shade "30x40")
(ii voice :shade "60x60")
(ii voice :shade "60x60" :blur "0x1")

