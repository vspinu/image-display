
(setq tt (create-image "img/test1.png"))


;;in %,  imagemagick convention
(image-transform tt :scale 200)
(image-transform tt :scale 25)

(insert-image (image-transform (copy-list tt) :resize '(500 . 500)))
(insert-image (image-transform (copy-list tt) :resize 200))

(insert-image (image-transform (copy-list tt) :resize 'fit-width))
(insert-image (image-transform (copy-list tt) :resize 'fit-height))
(insert-image (image-transform (copy-list tt) :resize 'fit))
(insert-image (image-transform (copy-list tt) :resize 'fit-stretch))
(insert-image (image-transform (copy-list tt) :resize 'fit-if-large))
(insert-image (image-transform (copy-list tt) :resize 'fit :rotate 45))
(insert-image (image-transform (copy-list tt) :resize 'fit-height :rotate 60))

(insert-image (image-transform (copy-list tt) :background "pink"))

(insert-image (image-transform (copy-list tt) :resize 'fit)
	      nil nil nil image-transform-map)



;; (image-transform-normalize:geometry :resize '(40 50 "!"))
;; (image-transform-normalize:geometry :resize '(40 50))
;; (image-transform-normalize:geometry :resize '(40 nil))
;; (image-transform-normalize:geometry :resize '(nil 40))
;; (image-transform-normalize:geometry :resize '(40 . 50))

;; (image-transform-normalize:scale :key 435)
;; (image-transform-normalize:scale :key "435%")
;; (image-transform-normalize:scale :key "sf435%")

;; (image-transform-parse:geometry "444")
;; (image-transform-parse:geometry "23x444!")
;; (image-transform-parse:geometry "x444")
;; (image-transform-parse:geometry "444x")
;; (image-transform-parse:geometry "x444")
;; (image-transform-parse:geometry "444@")
;; (image-transform-parse:geometry "444 555!")
;; (image-transform-parse:geometry "444x555%")

;; (image-tr--delete-transforms (list 'image :transforms (list 'ttt :key 34 :key 44)) '(:key))
