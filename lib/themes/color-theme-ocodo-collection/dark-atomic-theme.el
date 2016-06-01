(deftheme dark-atomic
  "2013-02-01 - Jason Milkins - A dark blue theme, designed for GUI use, based on subatomic-theme.el")

;; Originally based on subatomic-theme.el
;;  --- Nice looking emacs 24 theme

;; Copyright 2012 John Olsson

;; Author: John Olsson <john@cryon.se>
;; URL: https://github.com/cryon/subatomic
;; Version: 1.2

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

(custom-theme-set-faces
 'dark-atomic
 '(fringe ((t (:background "#151515"))))
 '(vertical-border ((t (:foreground "#2a2c3e"))))
 '(region ((t (:background "#696e92" :foreground "#ffffff"))))
 '(show-paren-match ((t (:foreground "#007700" :bold t))))
 '(show-paren-mismatch ((t (:foreground "#770000" :bold t))))
 '(isearch ((t (:background "#2a2c3e" :foreground "#009966" :bold t))))
 '(lazy-highlight ((t (:background "#2a2c3e" :foreground "#f95529" :bold t))))
 '(query-replace ((t (:inherit lazy-highlight))))
 '(trailing-whitespace ((t (:background nil :underline t :inherit show-paren-mismatch-face))))
 '(mode-line ((t (:box nil :foreground "#ffffff" :background "#2a2a2a" :family "Sans Serif" :height 0.8))))
 '(mode-line-inactive ((t (:box nil :foreground "#ffffff" :background "#232323" :family "Sans Serif" :height 0.8))))
 '(powerline-active1 ((t (:background "#2a2a2a"))))
 '(powerline-active2 ((t (:background "#2e2e2e"))))
 '(mode-line-inactive ((t (:background "#242434" :foreground "#696e92"))))
 '(powerline-inactive1 ((t (:background "#202030"))))
 '(powerline-inactive2 ((t (:background "#191919"))))
 '(header-line ((t (:background "#232533" :foreground "#ffffff" :weight bold))))
 '(hl-line ((t (:background "#2e3043"))))
 '(highlight-current-line-face ((t (:inherit hl-line))))
 '(minibuffer-prompt ((t (:foreground "#506490" :weight bold))))
 '(escape-glyph ((t (:foreground "#cebca5" :weight bold))))
 '(link ((t (:foreground "#9dbbd3" :weight bold :underline t))))
 '(font-lock-keyword-face ((t (:foreground "#2965b9" :weight bold))))
 '(font-lock-function-name-face ((t (:foreground "#8aa6bc"))))
 '(font-lock-warning-face ((t ((:foreground "#ea6633")))))
 '(font-lock-builtin-face ((t (:foreground "#3388dd"))))
 '(font-lock-variable-name-face ((t (:foreground "#8aa6bc"))))
 '(font-lock-constant-face ((t (:foreground "#ffffff" :weight bold :italic t))))
 '(font-lock-type-face ((t (:foreground "#9dbbd3" :weight bold))))
 '(font-lock-negation-char-face ((t (:foreground "#aa3333" :weight bold))))
 '(font-lock-preprocessor-face ((t (:foreground "#cebca5"))))
 '(font-lock-comment-face ((t (:foreground "#696e92"))))
 '(font-lock-string-face ((t (:foreground "#397c89"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#232533"))))
 '(font-lock-doc-face ((t (:foreground "#10404c" :italic t))))
 '(helm-candidate-number ((t (:foreground "black" :background "#779" ))))
 '(helm-selection ((t (:background "#122359" :underline nil ))))
 '(helm-separator ((t (:foreground "#117799" ))))
 '(helm-source-header ((t (:foreground "white" :background "nil" :weight bold :height 1.4 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:foreground "black" :background "#444"))))
 '(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
 '(eshell-ls-executable ((t (:foreground "#a9dc69"))))
 '(eshell-ls-directory ((t (:foreground "#8aa6bc" :bold t))))
 '(eshell-ls-archive ((t (:foreground "#f9b529"))))
 '(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
 '(eshell-ls-unreadable ((t (:inherit font-lock-warning-face))))
 '(eshell-ls-symlink ((t (:inherit font-lock-builtin-face))))
 '(eshell-prompt ((t (:inherit minibuffer-prompt))))
 '(eshell-ls-backup ((t (:foreground "#ea8673" :slant italic))))
 '(eshell-ls-product ((t (:inherit default :weight bold))))
 '(eshell-ls-readonly ((t (:inherit font-lock-comment))))
 '(eshell-ls-special ((t (:foreground "#cebca5"))))
 '(calendar-today-face ((t (:foreground "#a9dc69" :bold t))))
 '(holiday-face ((t (:foreground "#ea8673"))))
 '(diary-face ((t (:foreground "#9c71a5"))))
 '(erc-default-face ((t (:inherit default))))
 '(erc-current-nick-face ((t (:inherit font-lock-keyword-face))))
 '(erc-action-face ((t (:foreground "#cebca5"))))
 '(erc-dangerous-host-face ((t (:inherit font-lock-warning-face))))
 '(erc-highlight-face ((t (:weight bold))))
 '(erc-direct-msg-face ((t (:foreground "#a9dc69"))))
 '(erc-nick-msg-face ((t (:foreground "#9dbbd3" :weight bold))))
 '(erc-fool-face ((t (:inherit font-lock-comment-face))))
 '(erc-input-face ((t (:inherit default :weight bold))))
 '(erc-error-face ((t (:inherit font-lock-warning-face))))
 '(erc-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(erc-nick-default-face ((t (:inherit default))))
 '(erc-prompt-face ((t (:inherit eshell-prompt))))
 '(erc-notice-face ((t (:foreground "#9c71a5"))))
 '(erc-timestamp-face ((t (:inherit font-lock-comment-face))))
 '(erc-pal-face ((t (:foreground "#a9dc69"))))
 '(highlight-symbol-face ((t (:background "#232533"))))
 '(diff-file-header ((t (:background "#303347" :foreground "#8aa6bc"))))
 '(diff-header ((t (:inherit default :foreground "#696e92"))))
 '(diff-indicator-changed ((t (:foreground "#ffff00" :weight bold))))
 '(diff-changed ((t (:foreground "#f9b529"))))
 '(diff-indicator-removed ((t (:foreground "#ff0000" :weight bold))))
 '(diff-removed ((t (:foreground "#ea8673"))))
 '(diff-indicator-added ((t (:foreground "#00ff00" :weight bold))))
 '(diff-added ((t (:foreground "#a9dc69"))))
 '(diff-hunk-header ((t (:foreground "#ffffff"))))
 '(diff-refine-change ((t (:background "#232533" :foreground "#ffffff" :weight bold))))
 '(magit-branch ((t (:foreground "#a9dc69" :weight bold))))
 '(magit-diff-add ((t (:inherit diff-added))))
 '(magit-diff-del ((t (:inherit diff-removed))))
 '(magit-diff-file-header ((t (:inherit diff-file-header))))
 '(magit-diff-hunk-header ((t (:inherit diff-hunk-header))))
 '(magit-diff-none ((t (:inherit default))))
 '(magit-header ((t (:inherit diff-header))))
 '(magit-item-highlight ((t (:background "#2a2c3e"))))
 '(magit-item-mark ((t (:background "#2a2c3e"))))
 '(magit-log-graph ((t (:foreground "#8aa6bc"))))
 '(magit-log-head-label-bisect-bad ((t (:foreground "#ea8673"))))
 '(magit-log-head-label-bisect-good ((t (:foreground "#a9dc69"))))
 '(magit-log-head-label-default ((t (:foreground "#9c71a5" :weight bold))))
 '(magit-log-head-label-local ((t (:inherit magit-log-head-label-default :foreground "#a9dc69"))))
 '(magit-log-head-label-patches ((t (:inherit magit-log-head-label-default))))
 '(magit-log-head-label-remote ((t (:inherit magit-log-head-label-default))))
 '(magit-log-head-label-tags ((t (:inherit magit-log-head-label-default))))
 '(magit-log-message ((t (:inherit default))))
 '(magit-log-sha1 ((t (:foreground "#f9b529"))))
 '(magit-section-title ((t (:inherit header-line))))
 '(magit-whitespace-warning-face ((t (:inherit font-lock-warning))))
 '(compilation-info ((t (:inherit default))))
 '(compilation-warning ((t (:inherit font-lock-warning))))
 '(twittering-username-face ((t (:inherit font-lock-keyword-face))))
 '(twittering-uri-face ((t (:inherit link))))
 '(twittering-timeline-header-face ((t (:foreground "#cebca5" :weight bold))))
 '(twittering-timeline-footer-face ((t (:inherit twittering-timeline-header-face))))
 '(outline-1 ((t (:foreground "#f9b529" :weight bold))))
 '(outline-2 ((t (:foreground "#a9dc69" :weight bold))))
 '(outline-4 ((t (:foreground "#e5e5e5" :weight bold))))
 '(outline-3 ((t (:foreground "#cebca5" :weight bold))))
 '(outline-5 ((t (:foreground "#9dbbd3" :weight bold))))
 '(outline-6 ((t (:foreground "#9c71a5" :weight bold))))
 '(outline-7 ((t (:foreground "#81a257" :weight bold))))
 '(outline-8 ((t (:foreground "#696e92" :weight bold))))
 '(org-level-1 ((t (:inherit outline-1))))
 '(org-level-2 ((t (:inherit outline-2))))
 '(org-level-3 ((t (:inherit outline-3))))
 '(org-level-4 ((t (:inherit outline-4))))
 '(org-level-5 ((t (:inherit outline-5))))
 '(org-level-6 ((t (:inherit outline-6))))
 '(org-level-7 ((t (:inherit outline-7))))
 '(org-level-8 ((t (:inherit outline-8))))
 '(org-hide ((t (:foreground "#303347"))))
 '(org-link ((t (:inherit link))))
 '(org-checkbox ((t (:background "#303347" :foreground "#ffffff" :weight bold :box (:line-width 1 :style released-button)))))
 '(org-done ((t (:foreground "#a9dc69" :weight bold))))
 '(org-todo ((t (:foreground "#feccd4" :weight bold))))
 '(org-table ((t (:foreground "#cebca5"))))
 '(org-date ((t (:foreground "#feccd4" :weight bold))))
 '(org-document-info-keyword ((t (:foreground "#696e92"))))
 '(org-document-info ((t (:foreground "#cebca5" :weight bold :slant italic))))
 '(org-block-begin-line ((t (:background "#2a2c3e" :foreground "#696e92" :weight bold))))
 '(org-block-background ((t (:background "#2e3043"))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-agenda-date-today ((t (:foreground "#a9dc69" :background "#2a2c3e" :weight bold))))
 '(org-agenda-date ((t (:foreground "#9dbbd3"))))
 '(org-agenda-date-weekend ((t (:foreground "#feccd4"))))
 '(org-agenda-structure ((t (:inherit header-line))))
 '(org-warning ((t (:inherit font-lock-warning-face))))
 '(org-agenda-clocking ((t (:inherit org-date))))
 '(org-deadline-announce ((t (:inherit font-lock-warning-face))))
 '(org-formula ((t (:inherit font-lock-doc-face))))
 '(org-special-keyword ((t (:inherit font-lock-keyword))))
 '(diredp-compressed-file-suffix ((t (:foreground "#f9b529" :weight bold))))
 '(diredp-date-time ((t (:foreground "#696e92"))))
 '(diredp-deletion ((t (:foreground "#ea8673" :weight bold :slant italic))))
 '(diredp-deletion-file-name ((t (:foreground "#ea8673" :underline t))))
 '(diredp-symlink ((t (:foreground "#f9b529"))))
 '(diredp-dir-heading ((t (:inherit minibuffer-prompt))))
 '(diredp-display-msg ((t (:inherit default))))
 '(diredp-exec-priv ((t (:foreground "#a9dc69"))))
 '(diredp-write-priv ((t (:foreground "#ea8673"))))
 '(diredp-read-priv ((t (:foreground "#f9b529"))))
 '(diredp-dir-priv ((t (:foreground "#9dbbd3" :weight bold))))
 '(diredp-link-priv ((t (:foreground "#f9b529"))))
 '(diredp-other-priv ((t (:foreground "#f9b529" :weight bold))))
 '(diredp-rare-priv ((t (:foreground "#ea8673" :weight bold))))
 '(diredp-no-priv ((t (:foreground "#696e92"))))
 '(diredp-file-name ((t (:foreground "#e5e5e5"))))
 '(diredp-file-suffix ((t (:inherit dired-file-name))))
 '(diredp-number ((t (:foreground "#8aa6bc"))))
 '(diredp-executable-tag ((t (:foreground "#a9dc69" :weight bold))))
 '(diredp-flag-mark ((t (:bareground "#ea8673" :weight bold))))
 '(diredp-flag-mark-line ((t (:background "#2a2c3e"))))
 '(diredp-mode-line-marked ((t (:foreground "#ea8673"))))
 '(diredp-mode-line-flagged ((t (:foreground "#f9b529"))))
 '(diredp-ignored-file-name ((t (:foreground "#232533"))))
 `(linum ((t (:foreground "#434343" :background "#101010" :height 90 ))))
 '(default ((t (:background "#110F13" :foreground "#e5e5e5")))))

;; Rainbow delimiters
(defun dark-atomic-rainbow-delim-set-face ()
  (set-face-attribute 'rainbow-delimiters-depth-1-face nil :foreground "#4A7D90")
  (set-face-attribute 'rainbow-delimiters-depth-2-face nil :foreground "#2C4B56")
  (set-face-attribute 'rainbow-delimiters-depth-3-face nil :foreground "#1A2D33")
  (set-face-attribute 'rainbow-delimiters-depth-4-face nil :foreground "#237A9A")
  (set-face-attribute 'rainbow-delimiters-depth-5-face nil :foreground "#2C5C62")
  (set-face-attribute 'rainbow-delimiters-depth-6-face nil :foreground "#1A374D")
  (set-face-attribute 'rainbow-delimiters-depth-7-face nil :foreground "#135A8A")
  (set-face-attribute 'rainbow-delimiters-depth-8-face nil :foreground "#225C65")
  (set-face-attribute 'rainbow-delimiters-depth-9-face nil :foreground "#14373C")
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#660000"))

(eval-after-load "rainbow-delimiters" '(dark-atomic-rainbow-delim-set-face))

(provide-theme 'dark-atomic)
