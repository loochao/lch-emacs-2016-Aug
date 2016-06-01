(deftheme bright-meadow
  "Created 2012-07-09.")

(custom-theme-set-variables
 'bright-meadow
 '(linum-format "%7d")
 '(powerline-color1 "PaleGreen4")
 '(powerline-color2 "PaleGreen2")
 '(fringe-mode 9)
 '(current-language-environment "UTF-8")
 '(ediff-custom-diff-program "diff")
 '(ediff-diff-program "diff")
 '(ediff-diff3-program "diff3")
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote (quote right)))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(initial-scratch-message nil)
 '(linum-delay nil)
 '(linum-eager t)
 '(make-backup-files nil)
 '(markdown-command "redcarpet")
 '(markdown-css-path "/Users/jason/Projects/markdown-css-themes/markdown6.css")
 '(powerline-arrow-shape (quote (quote chamfer14)))
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(truncate-lines t)
 '(visible-bell t)
 '(global-visual-line-mode t)
 '(global-linum-mode t)
 '(global-hl-line-mode nil)
 '(cua-mode t)
 '(column-number-mode t)
 '(blink-cursor-mode nil))

(custom-theme-set-faces
 'bright-meadow
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(variable-pitch ((t (:family "Helvetica Neue"))))
 '(escape-glyph ((t (:background "grey30" :foreground "orange"))))
 '(linum ((t (:foreground "PaleGreen3" :background nil :box nil :height 100))))
 '(fringe ((t (:Foreground "PaleGreen3" :background "PaleGreen3"))))
 '(mode-line ((t (:background "PaleGreen2" :foreground "green4" :box nil :height 85))))
 '(mode-line-inactive ((t (:weight light :box nil :background "PaleGreen3" :foreground "DarkSeaGreen2" :inherit (mode-line)))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))
 '(mode-line-buffer-id ((t (:weight bold :box nil))))
 '(cursor ((t (:foreground "white" :background "orange"))))
 '(minibuffer-prompt ((t (:weight bold :foreground "DeepSkyBlue4"))))
 '(minibuffer-message ((t (:foreground "DodgerBlue4"))))
 '(region ((t (:background "grey40"))))
 '(secondary-selection ((t (:background "grey20"))))
 '(font-lock-builtin-face ((t (:foreground "DodgerBlue4"))))
 '(font-lock-comment-face ((t (:foreground "DarkOliveGreen3"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "DarkOliveGreen4"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "khaki4"))))
 '(font-lock-keyword-face ((t (:foreground "DeepSkyBlue4"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:inherit (font-lock-builtin-face)))))
 '(font-lock-regexp-grouping-backslash ((t (:inherit (bold)))))
 '(font-lock-regexp-grouping-construct ((t (:inherit (bold)))))
 '(font-lock-string-face ((t (:foreground "DodgerBlue4"))))
 '(font-lock-constant-face ((t (:foreground "snow4"))))
 '(font-lock-type-face ((t (:foreground "DarkSlateGray4"))))
 '(font-lock-variable-name-face ((t (:foreground "wheat4"))))
 '(font-lock-warning-face ((t (:weight bold :foreground "red"))))
 '(highlight ((((class color) (min-colors 88) (background light)) (:background "#003453")) (((class color) (min-colors 88) (background dark)) (:background "#003450")) (((class color) (min-colors 16) (background light)) (:background "#003450")) (((class color) (min-colors 16) (background dark)) (:background "#004560")) (((class color) (min-colors 8)) (:foreground "#000000" :background "#00FF00")) (t (:inverse-video t))))
 '(shadow ((((class color grayscale) (min-colors 88) (background light)) (:foreground "#999999")) (((class color grayscale) (min-colors 88) (background dark)) (:foreground "#999999")) (((class color) (min-colors 8) (background light)) (:foreground "#00ff00")) (((class color) (min-colors 8) (background dark)) (:foreground "#ffff00"))))
 '(trailing-whitespace ((((class color) (background light)) (:background "#ff0000")) (((class color) (background dark)) (:background "#ff0000")) (t (:inverse-video t))))
 '(link ((((class color) (min-colors 88) (background light)) (:underline t :foreground "#00b7f0")) (((class color) (background light)) (:underline t :foreground "#0044FF")) (((class color) (min-colors 88) (background dark)) (:underline t :foreground "#0099aa")) (((class color) (background dark)) (:underline t :foreground "#0099aa")) (t (:inherit (underline)))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:inherit (link))) (((class color) (background dark)) (:inherit (link)))))
 '(button ((t (:inherit (link)))))
 '(header-line ((default (:inherit (mode-line))) (((type tty)) (:underline t :inverse-video nil)) (((class color grayscale) (background light)) (:box nil :foreground "#222222" :background "#bbbbbb")) (((class color grayscale) (background dark)) (:box nil :foreground "#bbbbbb" :background "#222222")) (((class mono) (background light)) (:underline t :box nil :inverse-video nil :foreground "#000000" :background "#ffffff")) (((class mono) (background dark)) (:underline t :box nil :inverse-video nil :foreground "#ffffff" :background "#000000"))))
 '(tooltip ((default nil) (nil nil)))
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "#99ccee" :background "#444444")) (((class color) (min-colors 88) (background dark)) (:foreground "#bb3311" :background "##444444")) (((class color) (min-colors 16)) (:foreground "#0088cc" :background "#444444")) (((class color) (min-colors 8)) (:foreground "#0088cc" :background "#444444")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "#ffaaaa")) (((class color) (min-colors 88) (background dark)) (:background "#880000")) (((class color) (min-colors 16)) (:background "#FF0000")) (((class color) (min-colors 8)) (:background "#FF0000")) (((class color grayscale)) (:foreground "#888888")) (t (:inverse-video t))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "#77bbdd")) (((class color) (min-colors 88) (background dark)) (:background "#77bbdd")) (((class color) (min-colors 16)) (:background "#4499ee")) (((class color) (min-colors 8)) (:background "#4499ee")) (t (:underline t))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "#3388cc")) (((class color) (min-colors 88) (background dark)) (:background "#3388cc")) (((class color) (min-colors 8) (background light)) (:foreground "#000000" :background "#FFFF00")) (((class color) (min-colors 8) (background dark)) (:foreground "#ffffff" :background "#0000FF")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "Helvetica Neue"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 3.0))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 2.5))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 2.0))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.8))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.6))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.4))))
 '(default ((t (:background "#D6FFD6" :foreground "SpringGreen4")))))

(provide-theme 'bright-meadow)

