;;; init-site.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(defun znh/add-html-file (arg)
  (with-temp-buffer
    (insert-file-contents arg)
    (buffer-string)))

(defun znh/site-format-entry (entry style project)
  (format "[%s] [[file:%s][%s]] ---"
          (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))

(setq znh/site-path "D:\\AtoZ\\C_site\\")
(setq znh/site-publish-path (concat znh/site-path "HTML\\"))

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

(defun znh/site-format-entry (entry style project)
  (format "[%s] [[file:%s][%s]]"
          (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))

(setq znh/site-header-file (concat znh/site-path "templates\\header.html"))
(setq znh/site-footer-file (concat znh/site-path "templates\\footer.html"))

(setq org-publish-project-alist
      `(("post"
         :base-directory ,znh/site-path
         :base-extension "org"
         :publishing-directory ,znh/site-publish-path
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"./assets/style.css\" type=\"text/css\"/>"
         :html-preamble t
         :makeindex t
         ;; sitemap
         :sitemap-title "Archive"
         :sitemap-sort-files anti-chronologically
         :sitemap-format-entry znh/site-format-entry
         :auto-sitemap t
         :sitemap-filename "sitemap.org"
         ;; preamble and postamble
         :html-preamble ,(znh/add-html-file znh/site-header-file)
         :html-postamble ,(znh/add-html-file znh/site-footer-file))
        ("images"
         :base-directory  ,(expand-file-name "img/" znh/site-path)
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory ,(expand-file-name "img/" znh/site-publish-path)
         :publishing-function org-publish-attachment)
        ("other"
         :base-directory ,(expand-file-name "assets/" znh/site-path)
         :base-extension "css"
         :publishing-directory  ,(expand-file-name "assets/" znh/site-publish-path)
         :publishing-function org-publish-attachment)
        ("data"
         :base-directory ,(expand-file-name "data/" znh/site-path)
         :base-extension ".*"
         :publishing-directory  ,(expand-file-name "data/" znh/site-publish-path)
         :publishing-function org-publish-attachment)
        ("site" :components ("post" "images" "other" "data"))))


(setq org-html-doctype "html5"
      org-html-preamble t
      org-html-postamble t
      org-imenu-depth 5
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      ;; 让 Emacs 的 tag 位置不要太宽
      org-tags-column -50
      org-refile-targets '((nil :maxlevel . 9)
                           (znh/org-file :maxlevel . 9))
      org-image-actual-width 500
      org-export-use-babel nil)

(provide 'init-site)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-site.el ends here
