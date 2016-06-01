;;-*- coding:utf-8; mode:emacs-lisp; -*-

;;; OX
;;
;; Copyright (c) 2006 2007 2008 2009 2010 2011 Chao LU
;;
;; Author:  Chao LU <loochao@gmail.com>
;; URL:     http://www.princeton.edu/~chaol
;; Licence: GNU
;;
;; This file is not part of GNU Emacs.
;;
;; Commentary:
;; Control file of which one to load

;;; CODE
(message "=> lch-ox: loading...")

;;; Setting
;; special syntax for emphasized text
(setq org-emphasis-alist '(("*" bold "<b>" "</b>")
                           ("/" italic "<i>" "</i>")
                           ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                           ("=" org-code "<code>" "</code>" verbatim)
                           ("~" org-verbatim "<code>" "</code>" verbatim)
                           ("+" (:strike-through t) "<del>" "</del>")
                           ("@" org-warning "<b>" "</b>")))

;; alist of LaTeX expressions to convert emphasis fontifiers
(setq org-export-latex-emphasis-alist '(("*" "\\textbf{%s}" nil)
                                        ("/" "\\emph{%s}" nil)
                                        ("_" "\\underline{%s}" nil)
                                        ("+" "\\st{%s}" nil)
                                        ("=" "\\verb=%s=" nil)
                                        ("~" "\\verb~%s~" t)
                                        ("@" "\\alert{%s}" nil)))

(setq org-publish-timestamp-directory (concat org-dir "/.org-timestamps/"))
(setq org-export-exclude-tags (list "IDEA" "#A" "#B" "#C"))
;;; Publish
(require 'ox-publish)
(defvar lch-ox-html-postamble "<div id='hosted'><table><tr><td><a href='http://www.gnu.org/software/emacs/'><img src='./theme/emacs-logo.png' alt='Emacs' title ='Powered by GNU/Emacs' style='width:30px;'/></a></td> <td><a href='http://orgmode.org/'><img src='./theme/org-logo-unicorn.png' alt='Org' title='Powered by Emacs Org-mode'  style='width:30px;'/></a></td><td><a href='http://www.princeton.edu'><img src='./theme/PUTiger-logo.gif' alt='Princeton' title='Hosted by Princeton'  style='width:30px;'/></a></td></tr></table></div>")
(defvar lch-ox-html-head "<link rel=\"icon\" href=\"theme/favicon.ico\" type=\"image/x-icon\"/> <link rel=\"stylesheet\" href=\"./theme/loochao.css\"  type=\"text/css\"> </link>")

(setq org-publish-project-alist
      `(
        ("public-notes"
         ;; :author nil
         :base-directory ,org-source-dir
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :publishing-directory ,pub-html-dir
         :headline-levels 4
         :section-numbers nil
         :footnotes t
         :language "utf-8"
         :auto-sitemap t
         :sitemap-filename "Sitemap.org"
         :sitemap-title "LooChao's Homepage"
         :auto-preamble t
         :auto-postamble t
         :html-head ,lch-ox-html-head
         :html-postamble ,lch-ox-html-postamble
         :style-include-default nil
         )
        ("public-static"
         :base-directory ,org-source-dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|mp3\\|ogg\\|swf\\|ppt\\|md"
         :publishing-directory ,pub-html-dir
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("public" :components ("public-notes" "public-static"))

        ("options"
         :section-numbers nil
         :table-of-contents nil
         :html-head "<script type=\"text/javascript\">
                /* <![CDATA[ */
                org_html_manager.set("TOC", 1);
                org_html_manager.set("LOCAL_TOC", 1);
                org_html_manager.set("VIEW_BUTTONS", "true");
                org_html_manager.set("MOUSE_HINT", "underline"); // or background-color like '#eeeeee'
                org_html_manager.setup ();
                /* ]]> */
                </script>"
                )
        ("private"
         :base-directory ,org-private-dir
         :publishing-directory ,prv-html-dir
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :table-of-contents nil
         :html-head "<link rel=\"icon\" href=\"theme/favicon.ico\" type=\"image/x-icon\"/><link rel=\"stylesheet\" href=\"./theme/org.css\"  type=\"text/css\"> </link>"
         :auto-preamble t
         :auto-postamble t
         :auto-index t
         :index-filename "index.org"
         :index-title "LooChao's Private"
         :link-home "/index.html"
         )

        ("private"
         :base-directory ,org-private-dir
         :publishing-directory ,prv-html-dir
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :table-of-contents nil
         :html-head "<link rel=\"icon\" href=\"theme/favicon.ico\" type=\"image/x-icon\"/><link rel=\"stylesheet\" href=\"./theme/org.css\"  type=\"text/css\"> </link>"
         :auto-preamble t
         :auto-postamble t
         :auto-index t
         :index-filename "index.org"
         :index-title "LooChao's Private"
         :link-home "/index.html"
         )

        ("worg-notes"
         :base-directory ,worg-dir
         :publishing-directory ,worg-html-dir
         :base-extension "org"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :table-of-contents nil
         :html-head "<link rel=\"icon\" href=\"theme/favicon.ico\" type=\"image/x-icon\"/><link rel=\"stylesheet\" href=\"worg.css\"  type=\"text/css\"> </link>"
         :auto-preamble t
         :auto-postamble t
         ;; :auto-index t
         ;; :index-filename "index.org"
         ;; :index-title "Hello Worg"
         ;; :link-home "/index.html"
         )
        ("worg-static"
         :base-directory ,worg-dir
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|mp3\\|ogg\\|swf\\|ppt"
         :publishing-directory ,worg-html-dir
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("worg" :components ("worg-notes" "worg-static"))
        ))

;; (setq org-export-html-postamble-format
;;       '(("en" "<p class=\"postamble\">Last Updated %d. Created by %c"</p>)))

(defun lch-org-publish-org()
  (interactive)
  (org-publish-project
   (assoc "public" org-publish-project-alist)))

(defun lch-org-publish-prv()
  (interactive)
  (org-publish-project
   (assoc "private" org-publish-project-alist)))

(defun lch-org-publish-worg()
  (interactive)
  (org-publish-project
   (assoc "worg" org-publish-project-alist)))

;;; Org-page
;; Dependencies:
;; ----
;; emacs: this is an “of-course” dependency
;; org mode: v8.0 is required, please use M-x org-version <RET> to make sure you org mode version is not less than 8.0
;; git: a free and open source version control system
;; mustache.el: a mustache templating library for Emacs
;; htmlize.el: a library for syntax highlighting (usually this library is shipped with emacs)
;; dash.el: a modern list library for Emacs
;; ht.el: a modern hash-table library for Emacs
;;
;; Workflow
;; ----
;;

;; (require 'org-page)
;; (setq op/repository-directory "~/Dropbox/Org/org/")
;; (setq op/site-domain "http://www.princeton.edu/~chaol")
;; ;; (setq op/theme-root-directory "~/Dropbox/Org/org/themes")
;; (setq op/site-main-title "LooChao Notes")
;; (setq op/site-sub-title "LooChao")

;; (require 'org-page)
;; (setq op/personal-github-link "https://github.com/loochao")
;; (setq op/personal-disqus-shortname "loochao")
;; (setq op/personal-google-analytics-id "UA-41088132-1")
;; (setq op/category-config-alist
;;       '(("blog" ;; this is the default configuration
;;          :show-meta t
;;          :show-comment t
;;          :uri-generator op/generate-uri
;;          :uri-template "/blog/%y/%m/%d/%t/"
;;          :sort-by :date ;; how to sort the posts
;;          :category-index t) ;; generate category index or not
;;         ("wiki"
;;          :show-meta t
;;          :show-comment nil
;;          :uri-generator op/generate-uri
;;          :uri-template "/wiki/%t/"
;;          :sort-by :mod-date
;;          :category-index t)
;;         ("index"
;;          :show-meta nil
;;          :show-comment nil
;;          :uri-generator op/generate-uri
;;          :uri-template "/"
;;          :sort-by :date
;;          :category-index nil)
;;         ("about"
;;          :show-meta nil
;;          :show-comment nil
;;          :uri-generator op/generate-uri
;;          :uri-template "/about/"
;;          :sort-by :date
;;          :category-index nil)))

;; (setq op/theme 'mdo)
;; (setq op/personal-github-link "http://fort.kickass.systems:10082/cgit/")
;; (setq op/personal-google-analytics-id "UA-30162624-1")

;; ;; for commenting, you can choose either disqus or duoshuo
;; (setq op/personal-disqus-shortname "your_disqus_shortname")
;; (setq op/personal-duoshuo-shortname "your_duoshuo_shortname")

;; ;; the configuration below are optional
;; (setq op/personal-google-analytics-ida "your_google_analytics_id")

;;; One-key-map
(defvar one-key-menu-ox-alist nil
  "`One-Key' menu list for OX.")

(setq one-key-menu-ox-alist
      '(
        (("o" . "Org Publish") . org-publish)
        (("p" . "Publish Public") . lch-org-publish-org)
        (("P" . "Publish Private") . lch-org-publish-prv)
        (("w" . "Publish Worg") . lch-org-publish-worg)))

(defun one-key-menu-ox ()
  "`One-Key' menu for OX."
  (interactive)
  (one-key-menu "ox" one-key-menu-ox-alist t))

(define-key global-map (kbd "M-o") 'one-key-menu-ox)

;;; PROVIDE
(provide 'lch-ox)
(message "~~ lch-ox: done.")

;; Local Variables:
;; mode: emacs-lisp
;; mode: outline-minor
;; outline-regexp: ";;;;* "
;; End:
