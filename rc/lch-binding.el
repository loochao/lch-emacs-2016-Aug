;;-*- coding:utf-8; mode:emacs-lisp; -*-

;;; BINDINGS.EL
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Charles Lu
;;
;; Author:  Charles Lu <loochao@gmail.com>
;; URL:     http://www.princeton.edu/~chaol
;; License: GNU
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Global bindings
(message "=> lch-binding: loading...")

;;; REQUIRE
(require 'lch-key-util)                     ;Crucial To use lch-set-key
(require 'one-key)

;;; README
;; About command-map
;; Super and Meta are direct-command-map, like s-k is bound to 'kill-this-buffer
;; others are command-map-prefix
;; according how often the command will be used, it goes into those maps by this seq:
;; [C-c/C-x] => f1 => C-z => C-{. , / o}
;;; Guide-key
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("," "SPC" "g" "C-c" "C-z" "C-x r"
        "<f1>" "<f2>" "<f3>" "<f4>" "<f5>" "<f6>"
        "<f7>" "<f8>" "<f11>" "<f12>"))
(guide-key-mode 1)
(lch-diminish guide-key-mode " Ⓚ" " K")
;;; Mouse
(define-key global-map (kbd "<C-wheel-up>") 'text-scale-increase)
(define-key global-map (kbd "<C-wheel-down>") 'text-scale-decrease)

;;; Super (command-map)
(lch-set-key
 '(
   ("s-<f1>" . py-shell)
   ("s-<f2>" . matlab-shell)
   ("s-<f3>" . R)

   ("s-," . previous-buffer)
   ("s-." . next-buffer)
   ("s-=" . text-scale-increase)
   ("s--" . text-scale-decrease)

   ("s-k" . kill-this-buffer)
   ("s-s" . one-key-menu-w3m-search)
   ("s-u" . undo-kill-buffer)
   ("s-w" . kill-this-buffer)
   ))
;; One-key-menu-super
(defvar one-key-menu-super-alist nil "")
(setq one-key-menu-super-alist
      '(
        (("<f1>" . "python-shell") . py-shell)                                  ;; => lch-binding.el
        (("<f2>" . "matlab-shell") . matlab-shell)                              ;; => lch-binding.el
        (("<f3>" . "R") . R)                                                    ;; => lch-binding.el

        (("," . "prev-buffer") . previous-buffer)                               ;; => lch-binding.el
        (("." . "next-buffer") . next-buffer)                                   ;; => lch-binding.el

        (("=" . "magnify-font") . text-scale-increase)                          ;; => lch-binding.el
        (("-" . "demagnify-font") . text-scale-decrease)                        ;; => lch-binding.el

        (("7" . "tabbar-backward") . tabbar-backward)                           ;; => lch-ui.el
        (("8" . "tabbar-forward") . tabbar-forward)                             ;; => lch-ui.el
        (("9" . "tabbar-backward-group") . tabbar-backward-group)               ;; => lch-ui.el
        (("0" . "tabbar-forward-group") . tabbar-forward-group)                 ;; => lch-ui.el

        (("a" . "select all") . mark-whole-buffer)

        (("h" . "tabbar-backward") . tabbar-backward)                           ;; => lch-ui.el
        (("l" . "tabbar-forward") . tabbar-forward)                             ;; => lch-ui.el

        (("k" . "kill-this-buffer") . kill-this-buffer)                         ;; => lch-binding
        (("s" . "w3m-search") . one-key-menu-w3m-search)                        ;; => lch-binding.el

        (("w" . "kill-buffer") . kill-this-buffer)                              ;; => lch-binding.el
        (("<r,l,u,d>" . "emms-seek (-/+ 10/60)"))                               ;; => lch-emms.el
        ))

(defun one-key-menu-super ()
  "The `one-key' menu for SUPER."
  (interactive)
  (one-key-menu "SUPER" one-key-menu-super-alist t))
(define-key global-map (kbd "s-m") 'one-key-menu-super)

;;; Meta (command-map)
(lch-set-key
 '(
   ("M-`" . one-key-menu-term-scratch)
   ))

;; One-key-menu-meta
(defvar one-key-menu-meta-alist nil "")
(setq one-key-menu-meta-alist
      '(
        (("<f1>" . "one-key-help") . one-key-menu-help)                         ;; => lch-one-key.el
        (("<left>" . "hide-body") . hide-body)                                  ;; => lch-elisp.el
        (("<right>" . "show-all") . show-all)                                   ;; => lch-elisp.el
        (("<up>" . "outline-previous-heading") . outline-previous-heading)      ;; => lch-elisp.el
        (("<down>" . "outline-next-heading") . outline-next-heading)            ;; => lch-elisp.el
        (("<SPC>" . "helm") . helm-dwim)                                        ;; => lch-elisp.el
        (("/" . "dabbrev") . dabbrev-expand)                                    ;; => emacs-defaults
        (("'" . "yasnippet") . yas-expand)                                      ;; => lch-elisp.el
        (("`" . "multi-scratch-term") . one-key-menu-term-scratch)              ;; => lch-binding.el

        (("," . "scratch-prev") . multi-scratch-prev)                           ;; => lch-elisp.el
        (("." . "scratch-next") . multi-scratch-next)                           ;; => lch-elisp.el
        (("s" . "scratch-new") . multi-scratch-new)                             ;; => lch-elisp.el

        ;; (("`" . "menu-bar") . menu-bar)                                      ;; => emacs-defaults
        (("1" . "shell") . shell)                                               ;; => lch-binding.el
        (("2" . "lch-term") . lch-term)                                         ;; => lch-util.el
        (("3" . "lch-python") . lch-python)                                     ;; => lch-util.el
        (("4" . "lch-ruby") . lch-ruby)                                         ;; => lch-elisp.el
        (("5" . "lch-matlab") . lch-matlab)                                     ;; => lch-util.el
        (("6" . "lch-R") . lch-R)                                               ;; => lch-util.el
        (("7" . "lch-mathematica") . lch-mathematica)                           ;; => lch-util.el
        ;; (("2" . "multi-term") . multi-term-try-create)                       ;; => lch-binding.el

        (("8" . "org-agenda") . org-agenda)                                     ;; => lch-org.el
        (("9" . "erc-switch") . one-key-menu-irc-channel)                       ;; => lch-network.el
        ;; (("9" . "anything-menu") . one-key-menu-anything)                    ;; => lch-binding.el
        ;; (("a" . "anything-map") . anything)                                  ;; => lch-elisp.el
        (("k" . "one-key-kill") . one-key-menu-kill)                            ;; => lch-util.el
        (("o" . "org-export-menu") . one-key-menu-org-export)                   ;; => lch-org-export.el
        ))

(defun one-key-menu-meta ()
  "The `one-key' menu for META."
  (interactive)
  (one-key-menu "META" one-key-menu-meta-alist t))
(define-key global-map (kbd "M-m") 'one-key-menu-meta)

;; (defun lch-anything-menu ()
;;   "Generate one-key menu from anything-config.el"
;;   (with-temp-buffer
;;     (delete-region (point-min) (point-max))
;;     (insert-file (concat emacs-lisp-dir "/anything/anything-config.el"))
;;     (goto-char (point-min))
;;     (search-forward "define-key anything-command-map")
;;     (beginning-of-line)
;;     (kill-region (point-min) (point))
;;     (search-forward "'anything-c-run-external-command)")
;;     (end-of-line)
;;     (kill-region (point) (point-max))
;;     (goto-char (point-min))
;;     (while (search-forward "(define-key anything-command-map (kbd " nil t)
;;       (replace-match ""))
;;     (goto-char (point-min))
;;     (while (re-search-forward ") +'" nil t) (replace-match " . "))
;;     (goto-char (point-min))
;;     (while (re-search-forward "^\"" nil t) (replace-match "((\""))
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\. \\(.+\\))" nil t) (replace-match ". \\1) . \\1)"))
;;     (goto-char (point-min))
;;     (while (re-search-forward " \\. \\(.*?\\))" nil t) (replace-match " . \"\\1\")"))
;;     (goto-char (point-min))
;;     (while (re-search-forward ") \\. \"\\(.*?\\)\")" nil t) (replace-match ") . \\1)"))
;;     (goto-char (point-min))
;;     (insert "(defvar one-key-menu-anything-alist nil \"\")")
;;     (newline-and-indent)
;;     (insert "(setq one-key-menu-anything-alist")
;;     (newline-and-indent)

;;     (insert "'(")
;;     (goto-char (point-max))

;;     (newline-and-indent)
;;     (insert "))")
;;     (lch-indent-buffer)
;;     (eval-buffer)
;;     ))

;; (lch-anything-menu)
;; (defun one-key-menu-anything ()
;;   "The `one-key' menu for ANYTHING."
;;   (interactive)
;;   (one-key-menu "ANYTHING" one-key-menu-anything-alist t))
;; (define-key global-map (kbd "M-9") 'one-key-menu-anything)

;;; Ctrl (command-map)
(lch-set-key
 '(
   ("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)
   ("C-<f1>" . multi-term)
   ("C-<f2>" . multi-term-dedicated-toggle)
   ("C-<f3>" . multi-term-dedicated-select)
   ("C-2" . set-mark-command)
   ("C-o" . occur)
   ))

(defvar one-key-menu-ctrl-alist nil "")
(setq one-key-menu-ctrl-alist
      '(
        (("=" . "magnify-font") . text-scale-increase)                          ;; => lch-binding.el
        (("-" . "demagnify-font") . text-scale-decrease)                        ;; => lch-binding.el
        (("<f1>" . "multi-term") . multi-term)                                  ;; => lch-binding.el
        (("<f2>" . "mterm-dedicated-toggle") . multi-term-dedicated-toggle)     ;; => lch-binding.el
        (("<f3>" . "mterm-dedicated-select") . multi-term-dedicated-select)     ;; => lch-binding.el

        (("2" . "set-mark-command") . set-mark-command)                         ;; => lch-binding.el
        (("6" . "dired-jump") . dired-jump)                                     ;; => lch-dired.el
        (("o" . "occur") . occur)                                               ;; => lch-binding.el
        ))

(defun one-key-menu-ctrl ()
  "The `one-key' menu for CTRL."
  (interactive)
  (one-key-menu "CTRL" one-key-menu-ctrl-alist t))
(define-key global-map (kbd "C-M-m") 'one-key-menu-ctrl)

;;; C-x (command-map)
;; One-key-menu-ctrl-x
(lch-set-key
 '(
   ("C-x 4" . toggle-truncate-lines)
   ("C-x f" . ffap)
   ("C-x g" . goto-line)
   ("C-x u" . undo-tree-visualize)
   ("C-x C-b" . ibuffer)
   ))

(defvar one-key-menu-ctrl-x-alist nil "")
(setq one-key-menu-ctrl-x-alist
      '(
        (("4" . "toggle-truncate-lines") . toggle-truncate-lines)               ;; => lch-binding.el
        (("f" . "ffap") . ffap)                                                 ;; => lch-binding.el
        (("g" . "goto-line") . goto-line)                                       ;; => lch-binding.el
        (("u" . "undo-tree") . undo-tree-visualize)                             ;; => lch-elisp.el
        (("C-b" . "ibuffer") . ibuffer)                                         ;; => lch-elisp.el
        (("C-r" . "recentf") . recentf-open-files)                              ;; => lch-elisp.el
        ))

(defun one-key-menu-ctrl-x ()
  "The `one-key' menu for CTRL-X."
  (interactive)
  (one-key-menu "CTRL-X" one-key-menu-ctrl-x-alist t))
(define-key global-map (kbd "C-x m") 'one-key-menu-ctrl-x)

;;; C-c (command-map)
(lch-set-key
 '(
   ("C-c ." . repeat-complex-command)
   ("C-c c" . comment-region)                                                   ;; Shift+4 == $
   ("C-c d" . helm-dash)
   ("C-c g" . moccur-grep-find-pwd)
   ("C-c G" . grep-find)
   ("C-c l" . less-minor-mode)
   ("C-c o" . occur)
   ("C-c u" . uncomment-region)
   ;; ("C-c v" . toggle-viper-mode)
   ("C-c C-b" . list-bookmarks)
   ))

;; One-key-menu-ctrl-c
(defvar one-key-menu-ctrl-c-alist nil "")
(setq one-key-menu-ctrl-c-alist
      '(
        (("." . "repeat-complex-command") . repeat-complex-command)             ;; => lch-binding.el
        (("/" . "switch-to-message") . lch-switch-to-message)                   ;; => lch-util.el
        (("a" . "toggle-archive") . lch-toggle-archive)                         ;; => lch-util.el
        (("c" . "comment-region") . comment-region)                             ;; => lch-binding.el
        (("e" . "eval-buffer") . lch-eval-buffer)                               ;; => lch-util.el
        (("g" . "moccur-grep-pwd") . moccur-grep-find-pwd)                      ;; => lch-binding.el
        (("G" . "grep-find") . grep-find)                                       ;; => lch-binding.el
        (("i" . "indent-buffer-or-region") . lch-indent-region-or-buffer)       ;; => lch-util.el
        (("j" . "ace-jump") . ace-jump-mode)                                    ;; => lch-binding.el
        (("l" . "less") . less-minor-mode)                                      ;; => lch-binding.el
        (("o" . "occur") . occur)                                               ;; => lch-binding.el
        (("s" . "-> scratch") . lch-create-switch-scratch)                      ;; => lch-util.el
        (("u" . "uncomment-region") . uncomment-region)                         ;; => lch-binding.el
        (("v" . "evil") . evil-mode)                                            ;; => lch-binding.el
        (("y" . "helm-yas") . helm-c-yas-complete)                              ;; => lch-binding.el
        (("C-b" . "list-bookmarks") . list-bookmarks)                           ;; => lch-binding.el
        (("C-f" . "lch-sudo-edit") . lch-sudo-edit)                             ;; => lch-network.el
        (("R->" . "winner-redo") . winner-redo)                                 ;; => lch-ui.el
        (("L->" . "winner-undo") . winner-undo)                                 ;; => lch-ui.el
        ))

(defun one-key-menu-ctrl-c ()
  "The `one-key' menu for CTRL-C."
  (interactive)
  (one-key-menu "CTRL-C" one-key-menu-ctrl-c-alist t))
(define-key global-map (kbd "C-c m") 'one-key-menu-ctrl-c)

;;; C-z (command-map)
(lch-set-key
 '(
   ("C-z c" . count-words)
   ))
;; One-key-menu-ctrl-z
(defvar one-key-menu-ctrl-z-alist nil "")
(setq one-key-menu-ctrl-z-alist
      '(
        (("c" . "count-words") . count-words)                                       ;; => lch-binding.el
        (("d" . "org-drill") . org-drill)                                           ;; => lch-org.el
        (("f" . "fortune") . lch-echo-fortune)                                      ;; => lch-util.el
        (("v" . "clipboard") . view-clipboard)                                      ;; => lch-util.el
        ))
(defun one-key-menu-ctrl-z ()
  "The `one-key' menu for CTRL-Z."
  (interactive)
  (one-key-menu "CTRL-Z" one-key-menu-ctrl-z-alist t))
(define-key global-map (kbd "C-z m") 'one-key-menu-ctrl-z)

;;; Fn:  (command-map)
(lch-set-key
 '(
;;   ("<f4> <f4>" . kill-this-buffer)
   ))
(defvar one-key-menu-fn-alist nil "")
(setq one-key-menu-fn-alist
      '(
        (("<f1>" . "display-fn-keys") . lch-tip-of-the-day)                     ;; => lch-binding
        (("<f1> <f2>" . "start-terminal") . lch-start-terminal)                 ;; => lch-util.el
        (("<f2>" . "multi-term-new") . multi-term)                              ;; => lch-elisp.el
        (("<f2> <f1>" . "multi-term-prev") . multi-term-prev)                   ;; => lch-elisp.el
        (("<f2> <f3>" . "multi-term-next") . multi-term-next)                   ;; => lch-elisp.el
        (("<f3>" . "w3m") . lch-toggle-w3m)                                     ;; => lch-web.el
;;      (("<f4>" . "kill-buffer") . kill-this-buffer)                           ;; => lch-binding.el
        (("<f4>" . "goto-last-change") . goto-last-change)                      ;; => lch-elisp.el
        (("<f4> <f3>" . "thing-edit") . one-key-menu-edit)                      ;; => lch-one-key.el
        (("<f5>" . "bc-set") . bc-set)                                          ;; => lch-bmk.el
        (("<f6>" . "erc") . lch-erc-init)                                       ;; => lch-network.el
        (("<f7>" . "dictionary") . dictionary-search)                           ;; => lch-elisp.el
        (("<f9>" . "lch-start-file-browser") . lch-start-file-browser)          ;; => lch-util.el
        (("<f10>" . "open-dirs-w-emacs") . one-key-menu-f10s)                   ;; => lch-binding.el
        (("<f12>" . "emms") . lch-emms-init)                                    ;; => lch-emms.el
        (("C-<f9>" . "dired-single-magic") . dired-single-magic-buffer)         ;; => lch-dired.el
        (("<f9> <f10>" . "open-dirs-w-finder") . one-key-menu-df)               ;; => lch-binding.el
        (("Shift+ [l,r,u,d] ->" . "windmove") . zone)                           ;; => lch-elisp.el
        (("C-[mouse-scroll]" . "text-scale(+/-)") . zone)                       ;; => lch-binding.el
        ))

(defun one-key-menu-fn ()
  "The `one-key' menu for FN."
  (interactive)
  (one-key-menu "FN" one-key-menu-fn-alist t))
(define-key global-map (kbd "<f1> <f1>") 'one-key-menu-fn)

;;; F1:  (command-map)
(lch-set-key
 '(
   ("<f1> r" . re-builder)
   ("<f1> l" . locate)
   ("<f1> z" . zone)
   ))

;; One-key-menu-command
(defvar one-key-menu-command-alist nil "")
(setq one-key-menu-command-alist
      '(
        (("<f2>" . "start-terminal") . lch-start-terminal)                   ;; => lch-util.el
        (("c" . "smart-compile") . smart-compile)                            ;; => lch-elisp.el
        (("f" . "fortune") . lch-cowsay-fortune)                             ;; => lch-elisp.el
        (("l" . "locate") . locate)                                          ;; => lch-binding.el
        (("o" . "textmate") . lch-open-with-mate)                            ;; => lch-util.el
        (("r" . "re-builder") . re-builder)                                  ;; => lch-binding.el
        (("t" . "start-terminal") . lch-start-terminal)                      ;; => lch-util.el
        (("z" . "zone") . zone)                                              ;; => lch-binding.el
        ))

(defun one-key-menu-command ()
  "The `one-key' menu for COMMAND."
  (interactive)
  (one-key-menu "COMMAND" one-key-menu-command-alist t))
(define-key global-map (kbd "<f1> m") 'one-key-menu-command)

;;; F2:  (mode-map)
(lch-set-key
 '(
   ("<f2> a" . auto-complete-mode)
   ("<f2> A" . artist-mode)
   ("<f2> c" . calendar)
   ("<f2> C" . calc)
   ("<f2> f" . auto-fill-mode)
   ("<f2> l" . lisp-mode)
   ("<f2> o" . org-mode)
   ("<f2> O" . outline-minor-mode)
   ("<f2> s" . flyspell-mode)
   ("<f2> w" . whitespace-mode)
   ))

;; One-key-menu-mode
(defvar one-key-menu-mode-alist nil "")
(setq one-key-menu-mode-alist
      '(
        (("a" . "auto-complete") . auto-complete-mode)                          ;; => lch-binding.el
        (("A" . "artist") . artist-mode)                                        ;; => lch-binding.el
        (("c" . "calendar") . calendar)                                         ;; => lch-binding.el
        (("C" . "calc") . calc)                                                 ;; => lch-binding.el
        (("f" . "auto-fill-mode") . auto-fill-mode)                             ;; => lch-binding.el
        (("l" . "lisp-mode") . lisp-mode)                                       ;; => lch-binding.el
        (("o" . "org-mode") . org-mode)                                         ;; => lch-binding.el
        (("O" . "outline-mode") . outline-minor-mode)                           ;; => lch-binding.el
        (("p" . "paredit-mode") . paredit-mode)                                 ;; => lch-binding.el
        (("s" . "flyspell-mode") . flyspell-mode)                               ;; => lch-binding.el
        (("w" . "whitespace-mode") . whitespace-mode)                           ;; => lch-binding.el
        ))

(defun one-key-menu-mode ()
  "The `one-key' menu for MODE."
  (interactive)
  (one-key-menu "MODE" one-key-menu-mode-alist t))
(define-key global-map (kbd "<f2> m") 'one-key-menu-mode)
;;; F3:  (network-map)
;; Network and web related.
(defvar one-key-menu-network-alist nil
  "The `one-key' menu alist for NETWORK.")

(setq one-key-menu-network-alist
      '(
        (("<f2>" . "web-bookmark") . one-key-menu-web-bmk)                 ;; => lch-binding.el
        (("<f3>" . "w3m-toggle") . lch-toggle-w3m)                         ;; => lch-web.el
        (("<f4>" . "view-url-chrome") . lch-view-current-url-external)     ;; => lch-web.el
        (("<f6>" . "lch-erc-init") . lch-erc-init)                         ;; => lch-network.el
        (("<f7>" . "lch-erc-quit") . lch-erc-quit)                         ;; => lch-network.el
        (("d" . "wget") . wget)                                            ;; => lch-web.el
        (("e" . "lch-erc-init") . lch-erc-init)                            ;; => lch-network.el
        (("g" . "google") . lch-google)                                    ;; => lch-web.el
        (("s" . "w3m-search") . one-key-menu-w3m-search)                   ;; => lch-web.el
        ))

(defun one-key-menu-network ()
  "The `one-key' menu for NETWORK."
  (interactive)
  (one-key-menu "NETWORK" one-key-menu-network-alist t))
(define-key global-map (kbd "<f3> m") 'one-key-menu-network)

;; F3 F2: Web-bmk-map
;; Good webs goes to Bookmark.org, but best deserve a shortcut here.
(defvar one-key-menu-web-bmk-alist nil "")
(setq one-key-menu-web-bmk-alist
      '(
        (("s" . "slang") . (lambda () (interactive) (browse-url "http://www.urbandictionary.com/random.php")))
        ))

(defun one-key-menu-web-bmk ()
  "The `one-key' menu for F10S."
  (interactive)
  (one-key-menu "web-bmk" one-key-menu-web-bmk-alist t))
(define-key global-map (kbd "<f3> <f2>") 'one-key-menu-web-bmk)

;;; F4:  (buffer-edit-map)
(lch-set-key
 '(
   ("<f4> f" . fill-region)
   ("<f4> i" . indent-region)
   ))

(defvar one-key-menu-edit-alist nil
  "The `one-key' menu alist for EDIT.")

(setq one-key-menu-edit-alist
      '(
        (("<f3>" . "thing-edit") . one-key-menu-edit)                      ;; => lch-one-key.el
        (("3" . "copy-filename") . lch-copy-file-name-to-clipboard)        ;; => lch-util.el
        (("c" . "cleanup-buffer") . lch-cleanup-buffer)                    ;; => lch-util.el
        (("d" . "delete-buffer-n-file") . lch-delete-file-and-buffer)      ;; => lch-util.el
        (("f" . "fill-region") . fill-region)                              ;; => lch-binding.el
        (("i" . "indent-region") . indent-region)                          ;; => lch-binding.el
        (("k" . "kill-all-buffers") . kill-all-buffers)                    ;; => lch-util.el
        (("p" . "punctuate-buffer") . lch-punctuate-buffer)                ;; => lch-util.el
        (("r" . "rename-buffer-n-file") . lch-rename-file-and-buffer)      ;; => lch-util.el
        (("s" . "strip-blank-lines") . strip-blank-lines)                  ;; => lch-util.el
        (("t" . "insert-template") . template-expand-template)             ;; => lch-elisp.el
        ))

(defun one-key-menu-edit ()
  "The `one-key' menu for EDIT."
  (interactive)
  (one-key-menu "EDIT" one-key-menu-edit-alist t))
(define-key global-map (kbd "<f4> m") 'one-key-menu-edit)

;;; F5:  (bookmark-map)
(defvar one-key-menu-bmk-alist nil
  "The `one-key' menu alist for BMK.")

(setq one-key-menu-bmk-alist
      '(
        (("a" . "add-bookmark") . bookmark-set)                   ;; => lch-bmk.el
        (("b" . "list-bookmark") . list-bookmarks)                ;; => lch-bmk.el
        (("j" . "jump-to-bookmark") . switch-to-bookmark)         ;; => lch-bmk.el
        (("n" . "bc-local-next") . bc-local-next)                 ;; => lch-bmk.el
        (("p" . "bc-local-previous") . bc-local-previous)         ;; => lch-bmk.el
        (("g" . "bc-goto-current") . bc-goto-current)             ;; => lch-bmk.el
        (("l" . "bc-list") . bc-list)                             ;; => lch-bmk.el
        (("/" . "bc-list") . bc-list)                             ;; => lch-bmk.el
        (("," . "bc-previous") . bc-previous)                     ;; => lch-bmk.el
        (("." . "bc-next") . bc-next)                             ;; => lch-bmk.el
        (("'" . "bc-set") . bc-set)                               ;; => lch-bmk.el
        (("<f5>" . "bc-set") . bc-set)                            ;; => lch-bmk.el
        ))

(defun one-key-menu-bmk ()
  "The `one-key' menu for BMK."
  (interactive)
  (one-key-menu "BMK" one-key-menu-bmk-alist t))
(define-key global-map (kbd "<f5> m") 'one-key-menu-bmk)

;;; F6:
;; (setq one-key-menu-network-alist
;;       '(
;;         ))

;; (defun one-key-menu-network ()
;;   "The `one-key' menu for NETWORK."
;;   (interactive)
;;   (one-key-menu "NETWORK" one-key-menu-network-alist t))
;; (define-key global-map (kbd "<f6> m") 'one-key-menu-network)

;;; F7:  (dictionary-map)
(defvar one-key-menu-dict-alist nil
  "The `one-key' menu alist for DICT.")

(setq one-key-menu-dict-alist
      '(
        (("<f6>" . "dict-search") . dictionary-search)                    ;; => lch-elisp.el
        (("<f7>" . "dict-cn") . w3m-search-dict-cn)                       ;; => lch-web.el
        (("<f8>" . "dict-slang") . w3m-search-slang)                      ;; => lch-web.el
        ))

(defun one-key-menu-dict ()
  "The `one-key' menu for DICT."
  (interactive)
  (one-key-menu "DICT" one-key-menu-dict-alist t))
(define-key global-map (kbd "<f7> m") 'one-key-menu-dict)

;;; F8:
;;; F9:  (file-map)
(define-key global-map (kbd "<f9> 1") (lambda() (interactive) (dired org-source-dir)))

(define-key global-map (kbd "<f9> a") (lambda() (interactive) (find-file (concat org-source-dir "/Art-Ent.org"))))
(define-key global-map (kbd "<f9> b") (lambda() (interactive) (find-file (concat org-source-dir "/Bookmark.org"))))
(define-key global-map (kbd "<f9> B") (lambda() (interactive) (find-file (concat org-source-dir "/Bib-Edu.org"))))
(define-key global-map (kbd "<f9> c") (lambda() (interactive) (find-file (concat org-source-dir "/Culture.org"))))
(define-key global-map (kbd "<f9> C-c") (lambda() (interactive) (find-file (concat org-source-dir "/ComputerSE.org"))))
(define-key global-map (kbd "<f9> d") (lambda() (interactive) (find-file (concat emacs-doc-dir "/loochao-cheat-sheet.tex"))))
(define-key global-map (kbd "<f9> e") (lambda() (interactive) (find-file (concat org-source-dir "/Emacs.org"))))
(define-key global-map (kbd "<f9> E") (lambda() (interactive) (find-file (concat org-source-dir "/English.org"))))
(define-key global-map (kbd "<f9> C-e") (lambda() (interactive) (find-file (concat org-source-dir "/Economy.org"))))
(define-key global-map (kbd "<f9> g") (lambda() (interactive) (find-file (concat org-source-dir "/generality.org"))))
(define-key global-map (kbd "<f9> h") (lambda() (interactive) (find-file (concat org-source-dir "/Humor.org"))))
(define-key global-map (kbd "<f9> H") (lambda() (interactive) (find-file (concat org-source-dir "/Html.org"))))
(define-key global-map (kbd "<f9> C-h") (lambda() (interactive) (find-file (concat org-source-dir "/History.org"))))

(define-key global-map (kbd "<f9> i c") (lambda() (interactive) (find-file (concat org-private-dir "/iCount.org"))))
(define-key global-map (kbd "<f9> i d") (lambda() (interactive) (find-file (concat org-private-dir "/iDea.org"))))
(define-key global-map (kbd "<f9> i l") (lambda() (interactive) (find-file (concat org-private-dir "/iLog.org"))))
(define-key global-map (kbd "<f9> i n") (lambda() (interactive) (find-file (concat org-private-dir "/index.org"))))
(define-key global-map (kbd "<f9> i r") (lambda() (interactive) (find-file (concat org-private-dir "/iRsch.org"))))
(define-key global-map (kbd "<f9> i s") (lambda() (interactive) (find-file (concat org-private-dir "/iStuff.org"))))
(define-key global-map (kbd "<f9> i p") (lambda() (interactive) (find-file (concat org-private-dir "/iPrv.org"))))

(define-key global-map (kbd "<f9> l") (lambda() (interactive) (find-file (concat dropbox-path "/Library/Library.bib"))))
(define-key global-map (kbd "<f9> L") (lambda() (interactive) (find-file (concat org-source-dir "/Life.org"))))
(define-key global-map (kbd "<f9> C-l") (lambda() (interactive) (find-file (concat org-source-dir "/Library.org"))))
(define-key global-map (kbd "<f9> m") (lambda() (interactive) (find-file (concat org-source-dir "/Methodology.org"))))
(define-key global-map (kbd "<f9> M") (lambda() (interactive) (find-file (concat org-source-dir "/Mathematics.org"))))
(define-key global-map (kbd "<f9> C-m") (lambda() (interactive) (find-file (concat org-source-dir "/Miscellaneous.org"))))
(define-key global-map (kbd "<f9> O") (lambda() (interactive) (find-file (concat org-source-dir "/Opera.org"))))
(define-key global-map (kbd "<f9> p") (lambda() (interactive) (find-file (concat org-source-dir "/Pearl.org"))))
(define-key global-map (kbd "<f9> P") (lambda() (interactive) (find-file (concat org-source-dir "/Programming.org"))))
(define-key global-map (kbd "<f9> C-p") (lambda() (interactive) (find-file (concat org-source-dir "/iPU.org"))))
(define-key global-map (kbd "<f9> M-p") (lambda() (interactive) (find-file (concat org-source-dir "/Physics.org"))))
(define-key global-map (kbd "<f9> r") (lambda() (interactive) (find-file (concat dropbox-path "/Research/Research.bib"))))
(define-key global-map (kbd "<f9> R") (lambda() (interactive) (find-file (concat org-source-dir "/Refile.org"))))
(define-key global-map (kbd "<f9> s") (lambda() (interactive) (find-file (concat org-source-dir "/Softip.org"))))
(define-key global-map (kbd "<f9> t") (lambda() (interactive) (find-file (concat org-source-dir "/Travel.org"))))
(define-key global-map (kbd "<f9> S") (lambda() (interactive) (find-file (concat org-source-dir "/Sitemap.org"))))
(define-key global-map (kbd "<f9> u") (lambda() (interactive) (find-file (concat org-source-dir "/Unix.org"))))
(define-key global-map (kbd "<f9> W") (lambda() (interactive) (dired (concat dropbox-path "/GIT/Worg"))))

;;; F9s: (dir-finder-map)
(defvar one-key-menu-df-alist nil "")
(setq one-key-menu-df-alist
      '(
        (("l" . "libns-finder") . lch-open-libns-finder)
        (("s" . "scaned-notes-finder") . lch-open-pu-finder)
        (("w" . "libns-web-finder") . lch-open-libns-web-finder)
        ))

(defun one-key-menu-df ()
  "The `one-key' menu for dired-w-finder."
  (interactive)
  (one-key-menu "DF" one-key-menu-df-alist t))
(define-key global-map (kbd "<f9> <f10>") 'one-key-menu-df)

;;; F10: (file-map)
(define-key global-map (kbd "<f10> 1") (lambda() (interactive) (dired (concat emacs-dir "/rc"))))
(define-key global-map (kbd "<f10> 2") (lambda() (interactive) (dired dropbox-path)))
(define-key global-map (kbd "<f10> 3") (lambda() (interactive) (dired "~/Downloads")))
(define-key global-map (kbd "<f10> 4") (lambda() (interactive) (dired emacs-lisp-dir)))
(define-key global-map (kbd "<f10> 5") (lambda() (interactive) (dired emacs-lib-dir)))
;; various emacs.d from web
(defvar emacs-dz-dir (concat dropbox-path "/.emacs.dz"))    
(define-key global-map (kbd "<f10> 6") (lambda() (interactive) (dired emacs-dz-dir)))

(define-key global-map (kbd "<f10> b") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-binding.el"))))
(define-key global-map (kbd "<f10> B") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-bmk.el"))))
(define-key global-map (kbd "<f10> c") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-conf.el"))))
(define-key global-map (kbd "<f10> d") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-dired.el"))))
(define-key global-map (kbd "<f10> D") (lambda() (interactive) (find-file "~/.emacs")))
(define-key global-map (kbd "<f10> e") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-elisp.el"))))
(define-key global-map (kbd "<f10> E") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-evil.el"))))
(define-key global-map (kbd "<f10> C-e") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-env.el"))))
(define-key global-map (kbd "<f10> h") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-helm.el"))))
(define-key global-map (kbd "<f10> H") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-hydra.el"))))
(define-key global-map (kbd "<f10> i") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-init.el"))))
(define-key global-map (kbd "<f10> n") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-network.el"))))
(define-key global-map (kbd "<f10> o") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-org.el"))))
(define-key global-map (kbd "<f10> x") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-ox.el"))))
(define-key global-map (kbd "<f10> p") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-one-key.el"))))
(define-key global-map (kbd "<f10> t") (lambda() (interactive) (find-file (concat emacs-dir "/rc/color-theme-loochao.el"))))
(define-key global-map (kbd "<f10> u") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-util.el"))))
(define-key global-map (kbd "<f10> U") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-ui.el"))))

(defvar vimp-dir "~/vimperator")
(define-prefix-command 'f10-v-map)
(define-key global-map (kbd "<f10> v") 'f10-v-map)
(define-key global-map (kbd "<f10> v 1") (lambda() (interactive) (dired vimp-dir)))
(define-key global-map (kbd "<f10> v 2") (lambda() (interactive) (dired (concat vimp-dir "/colors"))))
(define-key global-map (kbd "<f10> v 3") (lambda() (interactive) (dired (concat vimp-dir "/plugin"))))
(define-key global-map (kbd "<f10> v 4") (lambda() (interactive) (dired (concat vimp-dir "/info"))))
(define-key global-map (kbd "<f10> v d") (lambda() (interactive) (find-file "~/.vimperatorrc")))
(define-key global-map (kbd "<f10> v i") (lambda() (interactive) (find-file (concat vimp-dir "/lch-init.vimp"))))
(define-key global-map (kbd "<f10> v k") (lambda() (interactive) (find-file (concat vimp-dir "/lch-key.vimp"))))
(define-key global-map (kbd "<f10> v p") (lambda() (interactive) (find-file (concat vimp-dir "/lch-plugin.vimp"))))
(define-key global-map (kbd "<f10> v u") (lambda() (interactive) (find-file (concat vimp-dir "/lch-ui.vimp"))))
(define-key global-map (kbd "<f10> v U") (lambda() (interactive) (find-file (concat vimp-dir "/lch-util.vimp"))))

(define-key global-map (kbd "<f10> V") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-var.el"))))
(define-key global-map (kbd "<f10> w") (lambda() (interactive) (find-file (concat emacs-dir "/rc/lch-web.el"))))
;; F10-end

;;; F10s (dir-map)
(defvar remote-boox-su "/scpc:root@10.0.0.18")
(defvar remote-notes "/scpc:chaol@nobel.princeton.edu:/u/chaol/Scan")
(defvar remote-mit "/scpc:jaimevrl@ab-initio.mit.edu:/home/jaimevrl")
(defvar remote-lib "/scpc:loochao@loochao.synology.me:/")
(defvar remote-chili "/scpc:chaol@chili.princeton.edu:/home/chaol")
(defvar remote-chili-su "/scpc:gradstudent@chili.princeton.edu:/home/chaol")


(defvar one-key-menu-rmt-alist nil "")
(setq one-key-menu-rmt-alist
      '(
        (("b" . "boox") . (lambda () (interactive) (dired-x-find-file (concat remote-boox-su "/data"))))        
        (("m" . "mit") . (lambda () (interactive) (dired-x-find-file (concat remote-mit "/chao/"))))
        (("c" . "chili") . (lambda () (interactive) (dired-x-find-file (concat remote-chili "/Downloads/"))))
        (("C" . "chili-su") . (lambda () (interactive) (dired-x-find-file (concat remote-chili-su "/Downloads/"))))
        (("e" . "emacs-rmt") . (lambda () (interactive) (dired-x-find-file (concat remote-notes "/ComputerSE/Emacs/"))))
        (("p" . "programming-rmt") . (lambda () (interactive) (dired-x-find-file (concat remote-notes "/Programming/"))))
        (("s" . "scaned_notes") . (lambda () (interactive) (dired-x-find-file remote-notes)))
        ))

(defun one-key-menu-rmt ()
  "The `one-key' menu for Remote Notes."
  (interactive)
  (one-key-menu "RMT" one-key-menu-rmt-alist t))
(define-key global-map (kbd "<f9> <f8>") 'one-key-menu-rmt)

(defvar netease-music-dir "/Volumes/DATA/Applications/Library/Netease_Music/")

(defvar one-key-menu-f10s-alist nil "")
(setq one-key-menu-f10s-alist
      '(
        (("<f10>" . "remote-notes-menu") . one-key-menu-rmt)
        (("c" . "code") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Code")))
        (("C" . "code2") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Code_B")))
        (("d" . "downloads") . (lambda () (interactive) (dired-x-find-file "~/Downloads")))
        (("e" . ".emacs.lib") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/.emacs.lib/")))
        (("f" . "ffx_profile") . (lambda () (interactive) (dired-x-find-file "/Volumes/DATA/Applications/Library/Application Supports/Firefox/")))
        (("F" . "flv") . (lambda () (interactive) (dired-x-find-file "/Volumes/DATA/Flv/")))
        (("h" . "public_html") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Org/public_html/")))
        (("g" . "git-repo") . (lambda () (interactive) (dired-x-find-file "/Volumes/DATA/Repository/")))
        (("l" . "library") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Library/")))
        (("m" . "music") . (lambda () (interactive) (dired-x-find-file "/Volumes/DATA/Music/")))
        (("n" . "netease-music") . (lambda () (interactive) (dired-x-find-file netease-music-dir)))
        (("N" . "remote-notes") . (lambda () (interactive) (dired-x-find-file remote-notes)))
        (("o" . "org") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Org/org/")))
        ;; (("p" . "PPTNotes") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/PPTNotes/")))
        (("p" . "ports") . (lambda () (interactive) (dired-x-find-file "/Volumes/DATA/Ports/")))
        (("P" . "paper") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Research/Papers2/Articles/")))
        (("r" . "research") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Research/")))
        (("s" . "snippet") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/.emacs.d/lib/snippets/lch/")))
        (("t" . "thesis") . (lambda () (interactive) (dired-x-find-file "~/Dropbox/Research/Thesis/")))
        (("v" . "volumes") . (lambda () (interactive) (dired-x-find-file "/Volumes/")))
        (("V" . "video") . (lambda () (interactive) (dired-x-find-file "/Volumes/DATA/Video/")))
        ))

(defun one-key-menu-f10s ()
  "The `one-key' menu for F10S."
  (interactive)
  (one-key-menu "F10S" one-key-menu-f10s-alist t))
(define-key global-map (kbd "<f10> <f10>") 'one-key-menu-f10s)
;;; F11: (ui-map)
(lch-set-key
 '(
   ("<f11> $" . toggle-truncate-lines)
   ("<f11> /" . eyedropper-foreground)
   ("<f11> b" . eyedropper-background)
   ("<f11> h" . global-hl-line-mode)
   ("<f11> l" . linum-mode)
   ("<f11> t" . tool-bar-mode)
   ))

;; One-key-menu-ui
(defvar one-key-menu-ui-alist nil "")
(setq one-key-menu-ui-alist
      '(
        (("$" . "line-truncate") . toggle-truncate-lines)                       ;; => lch-binding.el
        (("<f2>" . "color-theme-loochao") . color-theme-loochao)                ;; => lch-ui.el
        (("<f3>" . "color-theme-lazycat") . color-theme-lazycat)                ;; => lch-ui.el
        (("<f11>" . "toggle-fullscreen") . toggle-fullscreen)                   ;; => lch-ui.el
        (("1" . "cycle-fg-forward") . lch-cycle-fg-color-forward)               ;; => lch-ui.el
        (("2" . "cycle-bg-forward") . lch-cycle-bg-color-forward)               ;; => lch-ui.el
        (("3" . "frame-bg-pink") . lch-frame-pink)                              ;; => lch-ui.el
        (("4" . "frame-bg-black") . lch-frame-black)                            ;; => lch-ui.el
        (("/" . "eyedropper-foreground") . eyedropper-foreground)               ;; => lch-binding.el
        (("b" . "eyedropper-background") . eyedropper-background)               ;; => lch-binding.el
        (("a" . "ascii-on") . ascii-on)                                         ;; => lch-elisp.el
        (("A" . "ascii-off") . ascii-off)                                       ;; => lch-elisp.el
        (("f" . "fill-column-indicator") . fci-mode)                            ;; => lch-ui.el
        (("F" . "pick-face") . lch-face-at-point)                               ;; => lch-ui.el
        (("h" . "highlight-line") . global-hl-line-mode)                        ;; => lch-binding.el
        (("l" . "setnu-mode") . linum-mode)                                     ;; => lch-binding.el
        (("L" . "line-relative") . linum-relative-toggle)                       ;; => lch-binding.el
        (("M" . "hidden-mode-line") . hidden-mode-line-mode)                    ;; => lch-binding.el
        (("r" . "rainbow-mode") . rainbow-mode)                                 ;; => lch-ui.el
        (("t" . "tool-bar-mode") . tool-bar-mode)                               ;; => lch-binding.el
        ))

(defun one-key-menu-ui ()
  "The `one-key' menu for UI."
  (interactive)
  (one-key-menu "UI" one-key-menu-ui-alist t))
(define-key global-map (kbd "<f11> m") 'one-key-menu-ui)

;;; F12: (emms-map)

;; One-key-menu-emms
(defvar one-key-menu-emms-alist nil
  "The `one-key' menu alist for EMMS.")

(setq one-key-menu-emms-alist
      '(
        (("<f12>" . "emms-init") . lch-emms-init)
        (("<f11>" . "emms-dir-switch") . lch-emms-music-dir-switch)
        (("<f10>" . "emms-add-dir") . lch-emms-add-dir)
        (("<f9>" . "emms-play-file") . emms-play-file)
        (("<f8>" . "emms-add-playlist") . emms-add-playlist)
        (("SPC" . "toggle-playing") . lch-emms-toggle-playing)
        (("," . "emms-previous") . emms-previous)
        (("." . "emms-next") . emms-next)
        (("/" . "emms-show") . emms-show)
        (("c" . "emms-check-in") . lch-emms-check-in)
        (("d" . "emms-dump") . lch-emms-dump)
        (("i" . "emms-mode-line") . emms-mode-line-toggle)
        (("j" . "emms-jump-to-file") . emms-jump-to-file)
        (("l" . "lch-search-song-xiami") . lch-search-song-xiami)
        (("n" . "emms-next") . emms-next)
        (("p" . "emms-previous") . emms-previous)
        (("q" . "emms-quit") . lch-emms-quit)
        (("r" . "repeat-one") . emms-toggle-repeat-track)
        (("R" . "repeat-playlist") . emms-toggle-repeat-playlist)
        (("s" . "emms-shuffle") . lch-emms-shuffle)
        (("S" . "emms-stop") . emms-stop)
        (("x" . "emms-stop") . emms-stop)
        ))

(defun one-key-menu-emms ()
  "The `one-key' menu for EMMS."
  (interactive)
  (one-key-menu "EMMS" one-key-menu-emms-alist t))
(define-key global-map (kbd "<f12> m") 'one-key-menu-emms)

(lch-menu-to-key one-key-menu-emms-alist "<f12>")

;;; PROVIDE
(provide 'lch-binding)
(message "~~ lch-binding: done.")

;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
