;;; ELISP.EL
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Charles Lu
;;
;; Author:  Charles Lu <loochao@gmail.com>
;; URL:     http://www.princeton.edu/~chaol
;; License: GNU
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Settings for elisp packages.

;;; CODE
(message "=> lch-elisp: loading...")

;;; Emacs-package
(require 'package)

;; Define package repositories
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-user-dir (concat emacs-dir "/elpa"))

;; Load and activate emacs packages
(package-initialize)

;;; Rtf-mode
;; Does not work that well.
;; (autoload 'rtf-mode "rtf-mode" "RTF mode" t)
;; (add-to-list 'auto-mode-alist
;;   '("\\.rtf$" . rtf-mode))
;;; Projectile
;; Projectile is useful. Especially, projectile-replace and projectile-find-file.
;; Projectile commands are bound with the default C-c p prefix. So I can type C-c p C-h to list all of them.
(require 'projectile)
(projectile-global-mode +1)

(setq projectile-known-projects-file
      (expand-file-name "projectile-bookmarks.eld" emacs-var-dir))

(require 'diminish)
(diminish 'projectile-mode " PJ")
;;; Openwith
;; (require 'openwith)
;; (setq openwith-associations
;;             (list
;;              (list (openwith-make-extension-regexp
;;                     '("mpg" "mpeg" "mp3" "mp4"
;;                       "avi" "wmv" "wav" "mov" "flv"
;;                       "ogm" "ogg" "mkv"))
;;                    "/usr/bin/open"
;;                    '(file))
;;              (list (openwith-make-extension-regexp
;;                     '("xbm" "pbm" "pgm" "ppm" "pnm"
;;                       "png" "gif" "bmp" "tif" "jpeg" "jpg"))
;;                    "/usr/bin/open"
;;                    '(file))
;;              (list (openwith-make-extension-regexp
;;                     '("doc" "docx" "xls" "xlsx" "ppt" "pptx"))
;;                    "/usr/bin/open"
;;                    '(file))
;;              '("\\.lyx" "lyx" (file))
;;              (list (openwith-make-extension-regexp
;;                     '("pdf" "ps" "ps.gz" "dvi"))
;;                    "/usr/bin/open"
;;                    '(file))
;;              ))
;; (openwith-mode 1)
;; 
;;; Elfeed
;; Do not like the way it displays all the items, could not specify a certain newsfeed.
;;(require 'elfeed)
;;; Spray
;; (autoload 'spray "spray.el")
(require 'spray)
;; (setq spray-wpm 320)
(defun lch-init-spray ()
  (interactive)
  (evil-insert-state)
  (spray-mode t)
  ;; (evil-insert-state-cursor-hide)
  )

(define-key spray-mode-map (kbd "h") 'spray-backward-word)
(define-key spray-mode-map (kbd "l") 'spray-forward-word)
(define-key spray-mode-map (kbd "q")
        (lambda ()
          (interactive)
          (spray-quit)
          ;; (set-default-evil-insert-state-cursor)
          (evil-normal-state)))
(define-key global-map (kbd "C-z s") 'lch-init-spray)

;;; Diminish
;; Diminish some common modes; Diminishing a mode makes it eat less space in the
;; modeline, by replacing the text on the modeline with something else. The Emacs
;; config I stole these from uses a bunch of unicode characters, so I kind of
;; backfill that, too. I also hide some modes that I don't give any shits about.
;;
;; Has to be loaded in front part of this file.
(require 'diminish)
(lch-diminish auto-fill-function " Ⓕ" " F")

;;; Ace-window
(require 'ace-window)
;;; Zop-to-char
(require 'zop-to-char)
;; To replace `zap-to-char':
(define-key global-map (kbd "M-z") 'zop-to-char)

;;; Vi-tilde-fringe-mode
;; (require 'vi-tilde-fringe)
;; (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode)
;; (global-vi-tilde-fringe-mode)

;;; Popwin
(require 'popwin)
(popwin-mode 1)
;; (evil-leader/set-key "wpm" 'popwin:messages)
;; (evil-leader/set-key "wpp" 'popwin:close-popup-window)
(push '("*occur*"                    :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
(push '("*grep*"                     :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
(push '("*nosetests*"                :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
(push '("^\*Flycheck.+\*$" :regexp t :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
(push '("^\*WoMan.+\*$"    :regexp t              :position bottom                     ) popwin:special-display-config)
(defun spacemacs/remove-popwin-display-config (str)
  "Removes the popwin display configurations that matches the passed STR"
  (setq popwin:special-display-config
        (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                 (string-match str (car x))))
                 popwin:special-display-config)))
;;; Expand-region
(require 'expand-region)

;;; Guide-key-mode
;; in lch-binding.el

;;; Evernote
;; (require 'evernote-mode)
;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;; (defvar enh-enclient-command "/usr/texbin/enclient.rb" "Name of the enclient.rb command")
;; (setq evernote-developer-token "S=s1:U=baf7:E=1555c4f811b:C=14e049e5450:P=1cd:A=en-devtoken:V=2:H=7c434f23546a375d66c24668cf7a43e1")

;; (setq evernote-ruby-command "/Users/LooChao/.rvm/rubies/default/bin/ruby")
;; (setq enh-enclient-command "/Users/LooChao/.rvm/rubies/default/bin/enclient.rb")
;; (require 'evernote-mode)
;; (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
;; (custom-set-variables '(evernote-developer-token ""))
;; (add-to-list 'exec-path "~/.rvm/rubies/default/bin/")

;; (global-set-key "\C-cec" 'evernote-create-note)
;; (global-set-key "\C-ceo" 'evernote-open-note)
;; (global-set-key "\C-ces" 'evernote-search-notes)
;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
;; (global-set-key "\C-cew" 'evernote-write-note)
;; (global-set-key "\C-cep" 'evernote-post-region)
;; (global-set-key "\C-ceb" 'evernote-browser)

;;; Yasnippet
(require 'yasnippet)
(lch-diminish yas-minor-mode " Ⓨ" " Y")
(defvar snippet-root-dir (concat emacs-lib-dir "/snippets") "")

;; Order matters, put lch at last to overwrite others' conf if there's name
;; conflict.
(defvar andrea-snippet-dir (concat snippet-root-dir "/AndreaCrotti") "")
(defvar rejeep-snippet-dir (concat snippet-root-dir "/rejeep") "")
(defvar lch-snippet-dir (concat snippet-root-dir "/lch") "")
(defvar ocodo-snippet-dir (concat snippet-root-dir "/ocodo") "")

(add-to-list 'yas-snippet-dirs rejeep-snippet-dir)
(add-to-list 'yas-snippet-dirs andrea-snippet-dir)
(add-to-list 'yas-snippet-dirs lch-snippet-dir)
(add-to-list 'yas-snippet-dirs ocodo-snippet-dir)

(yas-global-mode 1)
;; has to disable under term-mode to make <tab> work.
(defun lch-force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))
(add-hook 'lch-force-yasnippet-off
              '(term-mode-hook shell-mode-hook))
;; (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1)))

(defun lch-reload-snippets ()
  (interactive)
  (dolist (x yas-snippet-dirs)
    (when (file-exists-p x)
      (yas-load-directory x))))

;; Better disable <tab>, otherwise conflict with terminal.
;; (define-key yas-minor-mode-map [(tab)] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "M-'") 'yas-expand)

;;; iDo
(ido-mode t)
(define-key global-map (kbd "C-x b") 'ido-switch-buffer)

;; Ignore .DS_Store files with ido mode
(add-to-list 'ido-ignore-files "\\.DS_Store")
(setq ido-save-directory-list-file (concat emacs-var-dir "/emacs-ido-last"))

;;; Goto-last-change
(require 'goto-chg)
(define-key global-map (kbd "C-x C-\\") 'goto-last-change)
(define-key global-map (kbd "<f4> <f4>") 'goto-last-change)
(define-key global-map (kbd "<f4> <f5>") 'goto-last-change-reverse)

;;; Auto-complete
;; Disable it usually, for it's slow and distracting.
;; Try dabbrev-expand (M-/)
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (require 'lch-key-util)
;; (ac-config-default)
;; (ac-flyspell-workaround)
;; (add-to-list 'ac-dictionary-directories (concat emacs-var-dir "/auto-complete-dict"))
;; (setq ac-comphist-file  (concat emacs-var-dir "/ac-comphist.dat"))

;; (global-auto-complete-mode t)
;; (setq ac-auto-show-menu t)
;; (setq ac-dwim t)
;; (setq ac-use-menu-map t)
;; (setq ac-quick-help-delay 1)
;; (setq ac-quick-help-height 60)
;; (setq ac-disable-inline t)
;; (setq ac-show-menu-immediately-on-auto-complete t)
;; (setq ac-auto-start 2)
;; (setq ac-candidate-menu-min 0)

;; (set-default 'ac-sources
;;              '(ac-source-dictionary
;;                ac-source-words-in-buffer
;;                ac-source-words-in-same-mode-buffers
;;                ac-source-semantic
;;                ac-source-yasnippet))

;; (dolist (mode '(org-mode text-mode lisp-mode markdown-mode))
;;   (add-to-list 'ac-modes mode))

;; Key triggers
;; (define-key ac-completing-map (kbd "M-.") 'ac-next)
;; (define-key ac-completing-map (kbd "M-,") 'ac-previous)
;; (define-key ac-completing-map "\t" 'ac-complete)
;; (define-key ac-completing-map (kbd "M-RET") 'ac-help)
;; (define-key ac-completing-map "\r" 'nil)

;;; AppleScript-mode
;; (autoload 'applescript-mode "applescript-mode"
;;   "Major mode for editing AppleScript source." t)
;; (add-to-list 'auto-mode-alist '("\\.applescript$" . applescript-mode))

;; Inf-Ruby
(autoload 'inf-ruby "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
(defun lch-ruby ()
  (interactive)
  (inf-ruby)
  (delete-other-windows))
;; (define-key global-map (kbd "M-5") 'lch-ruby)
;;; Coffee-mode
;; (require 'coffee-mode)
;; (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
;; (add-to-list 'auto-mode-alist '("\\.iced\\'" . coffee-mode))
;; (add-to-list 'auto-mode-alist '("Cakefile\\'" . coffee-mode))
;; (add-to-list 'interpreter-mode-alist '("coffee" . coffee-mode))
;;; iflipb
(require 'iflipb)
(define-key global-map (kbd "C-<f8>") 'iflipb-previous-buffer)
(define-key global-map (kbd "C-<f9>") 'iflipb-next-buffer)

;;; Multiple-cursor
(require 'multiple-cursors)
(define-key global-map (kbd "<f2> <f2>") 'mc/edit-lines)
;;; Magit
(require 'magit)
(define-key global-map (kbd "<f1> g") 'magit-status)
;;; Keyfreq
;; Use keyfreq-show to see how many times you used a command.
;; (require 'keyfreq)
;; (keyfreq-mode 1)
;; (keyfreq-autosave-mode 1)
;;; js2-mode
;; Said to be the best javascript mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
;;; Emmet-mode
;; Enable when editing HTML
;; (require 'emmet-mode)
;; (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
;; (add-hook 'html-mode-hook 'emmet-mode)
;; (add-hook 'css-mode-hook  'emmet-mode)
;;; Python-mode
(setq py-install-directory (concat emacs-lisp-dir "/python-mode/"))
(require 'python-mode)

;;; Multi-term/scratch
(require 'multi-term)
(require 'multi-scratch)

(define-key global-map (kbd "M-s") 'multi-scratch-new)
(define-key global-map (kbd "M-,") 'multi-scratch-prev)
(define-key global-map (kbd "M-.") 'multi-scratch-next)

(define-key global-map (kbd "M-n") 'multi-term)
(define-key global-map (kbd "M-[") 'multi-term-prev)
(define-key global-map (kbd "M-]") 'multi-term-next)

(defun lch-multi-term-disable-yas-minor-mode ()
  (yas-minor-mode -1))
(add-hook 'term-mode-hook 'lch-multi-term-disable-yas-minor-mode)

;; One-key-menu-term-scratch
(defvar one-key-menu-term-scratch-alist nil "")
(setq one-key-menu-term-scratch-alist
      '(
        (("3" . "mterm-dedicated-select") . multi-term-dedicated-select)            ;; => lch-binding.el

        (("[" . "mterm-prev") . multi-term-prev)                                    ;; => lch-binding.el
        (("]" . "mterm-next") . multi-term-next)                                    ;; => lch-binding.el

        (("," . "scratch-prev") . multi-scratch-prev)                               ;; => lch-binding.el
        (("." . "scratch-next") . multi-scratch-next)                               ;; => lch-binding.el
        (("s" . "scratch-new") . multi-scratch-new)                                 ;; => lch-binding.el

        (("c" . "mterm-dedicated-close") . multi-term-dedicated-close)              ;; => lch-binding.el
        (("o" . "mterm-dedicated-open") . multi-term-dedicated-open)                ;; => lch-binding.el

        (("n" . "mterm") . multi-term)                                              ;; => lch-binding.el
        (("t" . "mterm-dedicated-toggle") . multi-term-dedicated-toggle)            ;; => lch-binding.el
        ))

(defun one-key-menu-term-scratch ()
  "The `one-key' menu for TERM."
  (interactive)
  (one-key-menu "TERM" one-key-menu-term-scratch-alist t))
(define-key global-map (kbd "M-`") 'one-key-menu-term-scratch)

;;; ASCII
(autoload 'ascii-display "ascii" "" t)
(autoload 'ascii-on "ascii" "" t)
(autoload 'ascii-off "ascii" "" t)
(define-key global-map (kbd "<f11> a") 'ascii-on)
(define-key global-map (kbd "<f11> A") 'ascii-off)
;;; Calendar
(defun calendar-settings ()
  "settings for calendar mode"
  ;; required features
  (require 'cal-china-x)
  ;; settings
  (setq mark-holidays-in-calendar t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq calendar-holidays cal-china-x-important-holidays))

(add-hook 'calendar-load-hook 'calendar-settings)
;;; Smart-Compile
(autoload 'smart-compile "smart-compile" "" t)
(define-key global-map (kbd "M-<f2>") 'smart-compile)
(define-key global-map (kbd "<f1> c") 'smart-compile)
;;; Skeleton
;; Skeleton pair works with paredit, and more generally.
;; (setq skeleton-pair t)
;; (define-key global-map (kbd "(") 'skeleton-pair-insert-maybe)
;; (define-key global-map (kbd "[") 'skeleton-pair-insert-maybe)
;; (define-key global-map (kbd "{") 'skeleton-pair-insert-maybe)
;; (define-key global-map (kbd "\"") 'skeleton-pair-insert-maybe)
;; (define-key global-map (kbd "<") 'skeleton-pair-insert-maybe)

;; Weibo
(defun weibo-setting ()
  "settings for weibo"
  (setq weibo-directory "~/.emacs.d/WeiBo"))

(eval-after-load "weibo"
  '(weibo-setting))

(autoload 'weibo-timeline "weibo" nil t)

(defalias 'weibo 'weibo-timeline)
(define-key global-map (kbd "<f6> w") 'weibo)
;;; Eperiodic
(defun eperiodic-setting ()
  (custom-set-variables
   '(eperiodic-web-lookup-location "http://www.webelements.com/webelements/elements/text/%s/key.html")))

(eval-after-load "eperiodic"
  '(eperiodic-setting))

(autoload 'eperiodic "eperiodic"
  "Display the periodic table of the elements in its own buffer" t)

;;; Template
;; FIXME: template files need more adjustment.
(defconst emacs-template-dir (concat emacs-lib-dir "/templates"))
(defun template-setting ()
  "settings for tempalte"
  (template-initialize)
  (setq template-auto-insert 'query)
  (setq template-default-directories
        (cons emacs-template-dir template-default-directories)))

(eval-after-load "template"
  '(template-setting))

(autoload 'template-expand-template "template"
  "Expand template file TEMPLATE and insert result in current buffer" t)

(define-key global-map (kbd "<f4> t") 'template-expand-template)

;;; Smooth-scroll
;; Feel better when you use C-v.
(require 'smooth-scroll)
(smooth-scroll-mode 1)
(lch-diminish smooth-scroll-mode "")
;;; Unbound
(autoload 'describe-unbound-keys "unbound.el"
  "Display a list of unbound keystrokes of complexity no greater than MAX." t)
(define-key global-map (kbd "C-h u") 'describe-unbound-keys)
;;; Hexview
(autoload 'hexview-mode "hexview-mode"
  "Major mode for viewing file in hexical mode" t)
(define-key global-map (kbd "<f11> x") 'hexview-mode)
;;; Mathematica
(require 'mathematica)
(setq mathematica-command-line "/Applications/Mathematics/Mathematica.app/Contents/MacOS/MathKernel")
(define-key global-map (kbd "M-7") 'mathematica)
;;; ESS
(autoload 'ess-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics" t)
(autoload 'R "ess-site" "Emacs Speaks Statistics" t)
;; Slow setting (from manual).
;; (require 'ess-site)

;; (setq ess-ask-for-ess-directory nil)
;;; Dictionary
(autoload 'dictionary-search "dictionary" "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary "dictionary" "Create a new dictionary buffer" t)
(autoload 'dictionary-match-words "dictionary" "Ask for a word and search all matching words in the dictionaries" t)
(autoload 'dictionary-lookup-definition "dictionary" "Unconditionally lookup the word at point." t)
(autoload 'dictionary-mouse-popup-matching-words "dictionary" "Display entries matching the word at the cursor" t)
(autoload 'dictionary-popup-matching-words "dictionary" "Display entries matching the word at the point" t)
(autoload 'dictionary-tooltip-mode "dictionary" "Display tooltips for the current word" t)
(autoload 'global-dictionary-tooltip-mode "dictionary" "Enable/disable dictionary-tooltip-mode for all buffers" t)

(define-key global-map (kbd "<f7> <f6>") 'dictionary-search)

;;; Windmove
;; Part of GNU Emacs
;; Use shift + arrow to navigate between windows.
(require 'windmove)
(windmove-default-keybindings)
;;; Color-grep
(require 'color-grep)

;;; Go-to-char
(require 'go-to-char)

;;; Less-mode
(require 'less)
(require 'lch-key-util)
(define-key global-map (kbd "C-c l") 'less-minor-mode)
(defvar vi-move-key-alist nil
  "The key alist that like vi move.")
(setq vi-move-key-alist
      '(("j" . next-line)
        ("k" . previous-line)
        ("h" . backward-char)
        ("l" . forward-char)
        ("e" . scroll-down)
        ("SPC" . scroll-up)))
(lch-set-key vi-move-key-alist less-minor-mode-map)
(lch-set-key
 '(
   ("J" . less-scroll-up-one-line)
   ("K" . less-scroll-down-one-line)
   ("." . go-to-char-forward)
   ("," . go-to-char-backward)
   (">" . beginning-of-buffer)
   ("<" . end-of-buffer)
   ("q" . less-quit)
   ("b" . one-key-menu-hideshow)
   ("t" . one-key-menu-etags)
   )
 less-minor-mode-map
 )
;;; Rainbow-delimiter
(require 'rainbow-delimiters)
(define-key global-map (kbd "<f11> r") 'rainbow-delimiters-mode)  ;p stands for parenthesis
;;; Matlab
;; Work like a charm, but only enable when needed
(require 'matlab-load)

;;; Ace-jump-mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  ""
  t)
(define-key global-map (kbd "C-c j") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;; Savehist
;; keeps track of some history
;; Part of GNU Emacs
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(kill-ring search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat emacs-var-dir "/savehist"))
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(savehist-mode +1)

;;; Saveplace
;; Save point position between sessions
;; When you visit a file, point goes to the last place where it was
;; when you previously visited. Save file is set to emacs-var-dir/saveplace
(require 'saveplace)

;; activate it for all buffers
(setq-default save-place t)
(setq save-place-file (concat emacs-var-dir "/saveplace"))

;;; Volatile-highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)
;;; Cowsay-fortune
(require 'cowsay-fortune)
(defun lch-cowsay-fortune ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*fortune*"))
  (fortune-show))
(define-key global-map (kbd "<f1> f") 'lch-cowsay-fortune)
(when (eq (buffer-name) "*forture*")
  (setq buffer-read-only t))

(defvar lch-cowsay-switch t "")
(defun lch-toggle-cowsay ()
  (interactive)
  (when (and lch-cowsay-switch
             (file-exists-p "/usr/texbin/cowsay"))
    (run-with-idle-timer 600 t 'lch-cowsay-fortune)))
;; (lch-toggle-cowsay)
;;; Paredit
;; (autoload 'enable-paredit-mode "paredit"
;;   "Turn on pseudo-structural editing of Lisp code."
;;   t)
(require 'paredit)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(define-key global-map (kbd "<f2> p") 'paredit-mode)
(eval-after-load 'paredit
  '(lch-diminish paredit-mode " Ⓟ" " P"))

;;; Eye-dropper
;; To pick the fg and bg color at point
(require 'eyedropper)
;;; Thing-edit
;; Written by lazy cat, do sth after get things at point
;; An one key menu defined in lch-one-key.el
(require 'thing-edit)
;;; Flyspell

(require 'flyspell)
;; (diminish 'flyspell-mode " ✓")
(lch-diminish flyspell-mode " Ⓢ" " S")
;; (lch-diminish flyspell-mode " Ⓕ" " F")
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq-default ispell-program-name "aspell") ;; use aspell instead of ispell
(setq-default ispell-dictionary "english")

(defun lch-enable-flyspell ()
  (when (executable-find ispell-program-name)
    (flyspell-mode +1)))
(add-hook 'text-mode-hook 'lch-enable-flyspell)
(setq ispell-personal-dictionary (concat emacs-var-dir "/ispell_dict"))
;;; AucTeX
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq-default TeX-master nil)
(defun lch-latex-mode-hook ()
  (turn-on-auto-fill)
  (abbrev-mode +1)
  (flyspell-mode +1))
(add-hook 'LaTeX-mode-hook 'lch-latex-mode-hook)
;;; Anything
;; Anything.el provides a framework.
;; has to enable anything-config to make it really go.
;; anything-config is quite long and powerful.
;; (require 'anything)
;; (require 'anything-match-plugin)

;; (require 'anything-config)
;; (define-key global-map (kbd "M-SPC") 'anything)
;; (define-key global-map (kbd "M-a") 'anything-command-map)
;;; Outline
;; Outline is part of GNU Emacs
;; TODO: bind the outline-minor-mode-prefix C-c @ to C-o
(add-hook 'outline-minor-mode-hook 'hide-body)

;; Add hook to the following major modes so that the outline minor mode starts automatically.
;; Outline mode is better to be enabled only in document modes.
(defun lch-outline-mode ()
  (outline-minor-mode)
  (lch-diminish outline-minor-mode " Θ" " O"))

(dolist (hook
         '(emacs-lisp-mode-hook latex-mode-hook text-mode-hook change-log-mode-hook makefile-mode-hook))
  (add-hook hook 'lch-outline-mode))

(define-key global-map (kbd "M-<left>") 'hide-body)
(define-key global-map (kbd "M-<right>") 'show-all)
(define-key global-map (kbd "<left>") 'hide-entry)
(define-key global-map (kbd "<right>") 'show-entry)
(define-key global-map (kbd "M-<up>") 'outline-previous-heading)
(define-key global-map (kbd "M-<down>") 'outline-next-heading)

;;; Browse-kill-ring
;; (info "(emacs)Kill Ring")
(require 'browse-kill-ring)
(setq browse-kill-ring-separator
      "\n--item------------------------------")
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-highlight-inserted-item t)
(setq browse-kill-ring-highlight-current-entry t)
(setq browse-kill-ring-no-duplicates t)
(setq kill-ring-max 1024)

;;; Recentf
;; Part of GNU Emacs
(require 'recentf)

;; toggle `recentf' mode
(recentf-mode +1)

;; file to save the recent list into
(setq recentf-save-file (concat emacs-var-dir "/emacs.recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 30
      recentf-exclude '("/tmp/" "/ssh:"))

(add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")

(setq recentf-auto-cleanup 'never)
(setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))

;; lazy load recentf
(add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
;; Key bindings
(define-key global-map (kbd "C-x C-r") 'recentf-open-files)

;;; Undo-tree
;; Represent undo-history as an actual tree (visualize with C-x u)
(require 'undo-tree)
(global-undo-tree-mode 1)
(define-key global-map (kbd "C-x u") 'undo-tree-visualize)
(diminish 'undo-tree-mode)
;; (setq undo-tree-visualizer-timestamps t)
;; (setq undo-tree-visualizer-diff t)
(setq undo-tree-save-history t
      undo-tree-history-directory-alist `(("." . ,(concat emacs-var-dir "undo-tree-history"))))

;;; Uniquify
;; Make filename unique.
;; Part of GNU Emacs.
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;;; Rainbow-mode
;; Autoload to speed up.
(autoload 'rainbow-mode "rainbow-mode.el" "" t)
(define-key global-map (kbd "<f11> R") 'rainbow-mode)
;;; Live-fontify-hex
;; Display colors directly in lisp mode
;; Helpful when you write color-theme.
;; Enable only when work with color.
(require 'live-fontify-hex)
(font-lock-add-keywords 'lisp-mode
                        '((live-fontify-hex-colors)))
(font-lock-add-keywords 'emacs-lisp-mode
                        '((live-fontify-hex-colors)))
(font-lock-add-keywords 'lisp-interaction-mode
                        '((live-fontify-hex-colors)))
(font-lock-add-keywords 'css-mode
                        '((live-fontify-hex-colors)))

;;; Smex
(require 'smex)
(smex-initialize)
(setq smex-save-file (concat emacs-var-dir "/.smex-items"))
(define-key global-map (kbd "M-x") 'smex)
;; (define-key global-map (kbd "M-X") 'smex-major-mode-commands)
;; Old M-x.
(define-key global-map (kbd "C-c C-c M-x") 'execute-extended-command)

;;; Markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;; Whitespace-mode
;; Whitespace Mode lets you do a couple of neat things, reporting issues,
;; and fixing them.
(require 'whitespace)
;; (diminish 'whitespace-mode " ᗣ")
(lch-diminish whitespace-mode " Ⓦ" " W")
;; (diminish 'global-whitespace-mode " ᗣ")
;; (add-hook 'before-save-hook 'whitespace-cleanup)

;;; Switch-window
(require 'switch-window)

;;; mac-print
(when (require 'mac-print-mode nil t)
  (mac-print-mode 1)
  (global-set-key (kbd "<f1> p") 'mac-print-buffer))

;; Needed since mac-print will require htmlize
;; But htmlize won't work without code below.
(eval-after-load 'htmlize
  '(progn
     ;; make htmlize to handle face name strings as well
     (defadvice htmlize-attrlist-to-fstruct (around my-make-it-accept-string activate)
       (if (stringp (ad-get-arg 0))
           (progn
             (setq ad-return-value (htmlize-face-to-fstruct (intern (ad-get-arg 0)))))
         ad-do-it))))

(defvar my-htmlize-off-modes nil
  "list of minor modes to disable when using htmlize")

(defun my-htmlize-before-hook-default ()
  (dolist (mode my-htmlize-off-modes)
    (if (fboundp mode)
        (funcall mode 0)))

  (font-lock-fontify-buffer)
  (jit-lock-fontify-now)

  ;; copied from font-lock-default-function (make font-lock-face property act as alias for face property)
  (set (make-local-variable 'char-property-alias-alist)
       (copy-tree char-property-alias-alist))
  (let ((elt (assq 'face char-property-alias-alist)))
    (if elt
        (unless (memq 'font-lock-face (cdr elt))
          (setcdr elt (nconc (cdr elt) (list 'font-lock-face))))
      (push (list 'face 'font-lock-face) char-property-alias-alist))))

(add-hook 'htmlize-before-hook 'my-htmlize-before-hook-default)

;;; Desktop
;; Part of GNU Emacs
(require 'desktop)
(defun desktop-settings-setup()
  (desktop-save-mode 1)
  (setq desktop-save t)
  (setq desktop-load-locked-desktop t)
  (setq desktop-dirname emacs-var-dir)
  (setq desktop-path (list emacs-var-dir))
  (if (file-exists-p (concat desktop-dirname desktop-base-file-name))
      (desktop-read desktop-dirname)))

(add-hook 'after-init-hook 'desktop-settings-setup)


;;; PROVIDE
(provide 'lch-elisp)
(message "~~ lch-elisp: done.")

;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
