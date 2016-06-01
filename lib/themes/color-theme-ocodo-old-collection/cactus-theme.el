;;(defun color-theme-cactus ()
;;  (interactive)
;;  (color-theme-install
;;   '(color-theme-cactus
;;      ((background-color . "#000018")
;;      (background-mode . dark)
;;      (border-color . "#323232")
;;      (cursor-color . "#ffffff")
;;      (foreground-color . "#ffffff")
;;      (mouse-color . "black"))
;;     (fringe ((t (:background "#323232"))))
;;     (mode-line ((t (:foreground "#ffffff" :background "#323232"))))
;;     (region ((t (:background "#323232"))))
;;     (font-lock-builtin-face ((t (:foreground "#46b76d"))))
;;     (font-lock-comment-face ((t (:foreground "#36c36c"))))
;;     (font-lock-function-name-face ((t (:foreground "#ff792d"))))
;;     (font-lock-keyword-face ((t (:foreground "#b59973"))))
;;     (font-lock-string-face ((t (:foreground "#b59973"))))
;;     (font-lock-type-face ((t (:foreground"#e25734"))))
;;     (font-lock-constant-face ((t (:foreground "#eeeeec"))))
;;     (font-lock-variable-name-face ((t (:foreground "#eeeeec"))))
;;     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
;;     (font-lock-warning-face ((t (:foreground "red" :bold t))))
;;     )))
;;(provide 'color-theme-cactus)

(deftheme cactus
    "cactus")

(custom-theme-set-variables
  'cactus
     '(powerline-color1 "#00779a")
     '(powerline-color2 "#00475a")
     '(linum-format " %7i ")
     '(fringe-mode 6 nil (fringe))
     )


(custom-theme-set-faces
  'cactus

)


(provide-theme 'cactus)

