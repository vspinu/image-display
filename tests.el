
(require 'ert)
(require 'noflet)

(setq img (create-image (expand-file-name "img/test2.png")))
(setq butterfly (create-image (expand-file-name "img/butterfly.png")))
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
(insert-image (image-transform (copy-list img) :resize '(500 . 500)))
(insert-image (image-transform (copy-list img) :resize '(500 . 500) :backend 'convert))

(insert-image (image-transform (copy-list img) :resize 200 ))
(insert-image (image-transform (copy-list img) :resize 'fit-width))
(insert-image (image-transform (copy-list img) :resize 'fit-height))
(insert-image (image-transform (copy-list img) :resize 'fit))
(insert-image (image-transform (copy-list img) :resize 'fit-stretch))
(insert-image (image-transform (copy-list img) :resize 'fit-if-large))
(insert-image (image-transform (copy-list img) :resize 'fit :rotate 45))
(insert-image (image-transform (copy-list img) :resize 'fit-height :rotate 60))
(insert-image (image-transform (copy-list butterfly) :background "pink" :flatten :backend 'convert))
(insert-image (image-transform (copy-list img) :background "pink"))


;; map
(insert-image (image-transform (copy-list img) :resize 'fit)
	      nil nil nil image-manipulation-map)
;;; convert:
(setq image-transform-backends '(native))
(insert-image (image-transform-copy butterfly))

(insert-image (image-transform (copy-list img) :resize '(500 . 500)))
(insert-image (image-transform (copy-list img) :resize 200))
(insert-image (image-transform (copy-list img) :resize 'fit-width))
(insert-image (image-transform (copy-list img) :resize 'fit-height))
(insert-image (image-transform (copy-list img) :resize 'fit))
(insert-image (image-transform (copy-list img) :resize 'fit-stretch))
(insert-image (image-transform (copy-list img) :resize 'fit-if-large))
(insert-image (image-transform (copy-list butterfly) :resize 'fit-if-large))
(insert-image (image-transform (copy-list img) :background "transparent"))
(insert-image (image-transform (copy-list img) :resize 'fit-height :rotate 45))
(insert-image (image-transform (copy-list img) :resize 'fit :rotate 45))

(insert-image (image-transform (copy-list img) :resize 'fit-height :rotate 60))

;; flatten is needed for the background to take effect
(insert-image (image-transform (copy-list butterfly) :background "pink" :flatten))
(insert-image (image-transform (copy-list butterfly) :resize 'fit-height
			       :background "pink" :flatten))

;; (image-transform-spec:geometry :resize '(40 50 "!"))
;; (image-transform-spec:geometry :resize '(40 50))
;; (image-transform-spec:geometry :resize '(40 nil))
;; (image-transform-spec:geometry :resize '(nil 40))
;; (image-transform-spec:geometry :resize '(40 . 50))

;; (image-transform-spec:scale :key 435)
;; (image-transform-spec:scale :key "435%")
;; (image-transform-spec:scale :key "sf435%")

;; (image-transform-parse:geometry "444")
;; (image-transform-parse:geometry "23x444!")
;; (image-transform-parse:geometry "x444")
;; (image-transform-parse:geometry "444x")
;; (image-transform-parse:geometry "x444")
;; (image-transform-parse:geometry "444@")
;; (image-transform-parse:geometry "444 555!")
;; (image-transform-parse:geometry "444x555%")

;; (image-tr--delete-transforms (list 'image :transforms (list 'ttt :key 34 :key 44)) '(:key))
;; (setq tl '(:taint :taint :ping))
;; (image-tr--normalize-boolean-specs tl 'convert)
