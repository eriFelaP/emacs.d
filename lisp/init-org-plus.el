;;; init-org-plus.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; 辅助函数

(defun znh/add-html-file (arg)
  (with-temp-buffer
    (insert-file-contents arg)
    (buffer-string)))

(defun znh/site-format-entry (entry style project)
  (format "[%s] [[file:%s][%s]] ---"
          (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))

(defun znh/site-format-entry (entry style project)
  (format "[%s] [[file:%s][%s]]"
          (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))

;; 通用的设置

(setq org-html-mathjax-options
      '((path "./assets/MathJax-2.7.3/MathJax.js?config=TeX-AMS_HTML")
        (scale "100")
        (align "center")
        (font "TeX")
        (linebreaks "false")
        (autonumber "AMS")
        (indent "0em")
        (multlinewidth "85%")
        (tagindent ".8em")
        (tagside "right")))

(setq org-html-doctype "html5"
      org-html-head "<link rel=\"stylesheet\" href=\"./assets/style.css\" type=\"text/css\"/>"
      org-html-preamble t
      org-html-postamble "<div id=\"footer\">Created by <b>Zongnan HU</b> with <a href=\"https://orgmode.org/\">Org Mode</a></div>"
      org-imenu-depth 5
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      ;; 让 Emacs 的 tag 位置不要太宽
      org-tags-column -50
      org-refile-targets '((nil :maxlevel . 9)
                           (znh/org-file :maxlevel . 9))
      org-image-actual-width 500
      org-export-use-babel nil)

(setq org-plantuml-jar-path
      (expand-file-name "plantuml.jar"
                        user-emacs-directory))




;; 系统设置

;; slip-box 系统
(setq znh/slip-box "D:\\AtoZ\\C_slip_box\\")
(setq znh/slip-box-publish-path (concat znh/slip-box "HTML\\"))
(setq znh/slip-box-header-file (concat znh/slip-box "templates\\header.html"))

;; blog 系统
(setq znh/blog "D:\\AtoZ\\E_blog\\")
(setq znh/blog-publish-path (concat znh/blog "HTML\\"))
(setq znh/blog-header-file (concat znh/blog "templates\\header.html"))

(setq org-default-notes-file (expand-file-name "note.org" znh/blog))
(setq org-agenda-files `(,(expand-file-name "todos.org" znh/blog)
                         ,(expand-file-name "projects.org" znh/blog)))


(setq org-publish-project-alist
      `((;;; slip-box 系统
         "slip-box-post"
         :base-directory ,znh/slip-box
         :base-extension "org"
         :publishing-directory ,znh/slip-box-publish-path
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-preamble t
         :makeindex nil
         ;; sitemap
         :sitemap-title "网站地图"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry znh/site-format-entry
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         ;; preamble and postamble
         :html-preamble ,(znh/add-html-file znh/slip-box-header-file))
        ("slip-box-images"
         :base-directory  ,(expand-file-name "img/" znh/slip-box)
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory ,(expand-file-name "img/" znh/slip-box-publish-path)
         :publishing-function org-publish-attachment)
        ("slip-box-other"
         :base-directory ,(expand-file-name "assets/" znh/slip-box)
         :base-extension "css"
         :publishing-directory  ,(expand-file-name "assets/" znh/slip-box-publish-path)
         :publishing-function org-publish-attachment)
        ("slip-box-data"
         :base-directory ,(expand-file-name "data/" znh/slip-box)
         :base-extension ".*"
         :publishing-directory  ,(expand-file-name "data/" znh/slip-box-publish-path)
         :publishing-function org-publish-attachment)
        ("slip-box" :components ("slip-box-post" "slip-box-images"
                                 "slip-box-other" "slip-box-data"))
;;; blog 系统
        ("blog-post"
         :base-directory ,znh/blog
         :base-extension "org"
         :publishing-directory ,znh/blog-publish-path
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-preamble t
         :makeindex nil
         ;; sitemap
         :sitemap-title "网站地图"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry znh/site-format-entry
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         ;; preamble and postamble
         :html-preamble ,(znh/add-html-file znh/blog-header-file))
        ("blog-images"
         :base-directory  ,(expand-file-name "img/" znh/blog)
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory ,(expand-file-name "img/" znh/blog-publish-path)
         :publishing-function org-publish-attachment)
        ("blog-other"
         :base-directory ,(expand-file-name "assets/" znh/blog)
         :base-extension "css"
         :publishing-directory  ,(expand-file-name "assets/" znh/blog-publish-path)
         :publishing-function org-publish-attachment)
        ("blog-data"
         :base-directory ,(expand-file-name "data/" znh/blog)
         :base-extension ".*"
         :publishing-directory  ,(expand-file-name "data/" znh/blog-publish-path)
         :publishing-function org-publish-attachment)
        ("blog" :components ("blog-post" "blog-images"
                             "blog-other" "blog-data"))))

;; 加快打开 org 文件的速度
(when (and (fboundp 'daemonp) (daemonp))
  (find-file-noselect
   (expand-file-name "index.org" znh/slip-box))
  (org-publish-all))


;; 使用 deft 来管理 slip-box
(require-package 'deft)

(with-eval-after-load 'deft
  (setq deft-new-file-format "%Y-%m-%dT%H%M"
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename nil
        deft-org-mode-title-prefix t
        deft-directory znh/slip-box
        deft-default-extension "org"
        deft-extensions '("org" "md")
        deft-auto-save-interval 60))



(provide 'init-org-plus)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-org-plus.el ends here
