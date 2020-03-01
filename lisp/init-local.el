;;; init-local.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; 每次启动的时候随机显示一些格言
(defun znh/motto-of-the-day ()
  "Display a random entry from mottos."
  (interactive)
  (let ((mottos `("行易知难。"
                  ;; （孙中山《孙文学说》）
                  "图难于其易，为大于其细。"
                  ;;（老子《道德经·六十三章》)
                  "你们不要论断人，免得你们被论断。"
                  ;; (马太福音 7:1 圣经和合本)
                  "Simple living, high Thinking."
                  ;; （Mahatma Gandhi）
                  "离虚诳语、离粗恶、离离间语、离杂秽语。"
                  ;; 《大般若波羅蜜多經》卷341,CBETA, T06, no. 220, p. 749c8-9)
                  "你们的话，是，就说是；不是，就说不是；若再多说就是出于那恶者。"
                  ;; （马太福音 5:37 和合本）
                  "大器晚成。"
                  ;; （老子《道德经》）
                  "有何胜利可言？挺住就是一切。"
                  ;; （里尔克）
                  "当我们等着瞧那最末的日子的时候，不要说一个凡人是幸福的。"
                  ;; （《俄狄浦斯王》）
                  "疯狂就是重复做相同的事情却期待不同的结果。"
                  ;; 佚名（不是爱因斯坦说的）
                  "日拱一卒，功不唐捐。"
                  )))
    (random t)
    (message (concat "  < "
                     (nth (random (length mottos))
                          mottos)
                     " > "))))

(defun prelude-eval-after-init (form)
  "Add `(lambda () FORM)' to `after-init-hook'.
If Emacs has already finished initialization, also eval FORM immediately."
  (let ((func (list 'lambda nil form)))
    (add-hook 'after-init-hook func)
    (when after-init-time
      (eval form))))

(prelude-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'znh/motto-of-the-day))

;;----------------------------------------------------------------------
;; Emacs 默认的鼠标滚动体验很差。下面配置可以让鼠标滚动更加顺滑。
;;----------------------------------------------------------------------
(setq mouse-wheel-scroll-amount
      '(1                                ; 一次只滚动 1 行
        ((shift) . 2)                    ; 按住 Shift 滚动 2 行
        ((control). 3))                  ; 按住 Ctrl 滚动 3 行
      mouse-wheel-progressive-speed nil)  ; 滚动过程不加速


;;----------------------------------------------------------------------
;; 启用文本拖拽，就像 Word 一样。这样在写作的时候更加友好。
;;----------------------------------------------------------------------
(setq  mouse-drag-and-drop-region t
       mouse-drag-and-drop-region-cut-when-buffers-differ t)

;;----------------------------------------------------------------------
;; 分别设置中英文字体。Emacs 默认的中文字体不仅丑陋，还会让 Emacs 卡顿。
;; 设置中文和英文不同的缩放率可以让 Org Mode 的表格对齐。
;;
;; 注意这里最好用宋体。如果是黑体，缩放后会显得非常大。
;;----------------------------------------------------------------------
(when window-system
  (set-face-attribute 'default nil :font "Noto Sans Mono 10")
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family "Noto Serif CJK SC Medium")))
  (setq face-font-rescale-alist '(("Noto Sans Mono" . 1)
                                  ("Noto Serif CJK SC Medium" . 1.2))))


;;----------------------------------------------------------------------
;; 把 C-SPC 留个系统输入法切换；把 Emacs 内置输入法切换换成设置标记。
;;----------------------------------------------------------------------
(global-unset-key (kbd "C-SPC"))

(if window-system
    (global-set-key (kbd "C-\\")    #'set-mark-command)
  (global-unset-key (kbd "C-\\")))


;;----------------------------------------------------------------------
;; 配置一些常用的全局快捷键。
;;----------------------------------------------------------------------
(global-set-key (kbd "C-x \\") #'align-regexp)
(global-set-key (kbd "M-o") #'switch-window)
(global-set-key (kbd "M-g c") #'avy-goto-char)

(defalias 'rc 'revert-buffer-with-coding-system)
(defalias 'sc 'set-buffer-file-coding-system)
(defalias 'ds 'desktop-save)
(defalias 'dc 'desktop-change-dir)

(maybe-require-package 'easy-kill)

(global-set-key (kbd "M-\\") 'easy-mark)


;;-----------------------------------------------------------------------
;; org mode
;;-----------------------------------------------------------------------

(maybe-require-package 'olivetti)
(require 'olivetti)
(defun znh/disable-word-wrap (&rest args)
  "Disable word wrap.  ARGS."
  (setq word-wrap nil))
(advice-add 'olivetti-mode :after #'znh/disable-word-wrap)
(add-hook 'org-mode-hook
          #'olivetti-mode)


;;----------------------------------------------------------------------
;; 我的笔记系统
;;----------------------------------------------------------------------
(setq znh/note-dir
      "D:\\orgcards")

(setq znh/note-html-dir
      "D:\\orgcards_html")

(defun znh/get-note-abspaths ()
  "Get note abspaths."
  (let ((paths (directory-files
                (file-name-as-directory znh/note-dir)
                nil
                "^[0-9]+-[0-9]+-[0-9]+-[0-9]+-[0-9]+-[0-9]+\.org$"))
        (abspaths '()))
    (dolist (path paths abspaths)
      (setq abspaths
            (cons (expand-file-name
                   path
                   (file-name-as-directory znh/note-dir))
                  abspaths)))))

(defun znh/note ()
  "Take note."
  (interactive)
  (let ((filename (format-time-string "%F-%H-%M-%S.org"))
        (root (file-name-as-directory znh/note-dir)))
    (find-file (expand-file-name filename root))))


(defun znh/random-choice-card ()
  "Random choice note."
  (interactive)
  (let* ((notes (znh/get-note-abspaths))
         (ns (length notes)))
    (if notes
        (find-file (nth (random ns) notes))
      (znh-card))))

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

(setq org-publish-project-alist
      `(("orgfiles"
         :base-directory ,znh/note-dir
         :base-extension "org"
         :publishing-directory ,znh/note-html-dir
         :publishing-function org-html-publish-to-html
         :headline-levels 3
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"./assets/style.css\" type=\"text/css\"/>"
         :html-preamble t
         :makeindex t)
        ("images"
         :base-directory  ,(expand-file-name "img/" znh/note-dir)
         :base-extension "jpg\\|gif\\|png"
         :publishing-directory ,(expand-file-name "img/" znh/note-html-dir)
         :publishing-function org-publish-attachment)
        ("other"
         :base-directory ,(expand-file-name "assets/" znh/note-dir)
         :base-extension "css"
         :publishing-directory  ,(expand-file-name "assets/" znh/note-html-dir)
         :publishing-function org-publish-attachment)
        ("website" :components ("orgfiles" "images" "other"))))

(setq org-html-doctype "html5"
      org-html-preamble nil
      org-html-postamble nil
      org-imenu-depth 5
      org-refile-use-outline-path t
      org-outline-path-complete-in-steps nil
      ;; 让 Emacs 的 tag 位置不要太宽
      org-tags-column -50
      org-refile-targets '((nil :maxlevel . 9)
                           (znh/org-file :maxlevel . 9))
      org-image-actual-width 500)

;; 使用中文的格式
(setq system-time-locale "C")

;; 使用 org-download

(maybe-require-package 'org-download)

(when (eq system-type 'windows-nt)
  (setq org-download-screenshot-file
        "D:\\temp\\screenshot.png"
        org-download-screenshot-method
        "\"D:\\Program Files\\IrfanView\\i_view64.exe\" /capture=4 /convert=\"%s\""))

(setq-default org-download-heading-lvl nil)
(setq-default org-download-image-dir "./img")

(add-hook 'org-mode-hook
          #'org-download-enable)

(after-load 'org-download
  (define-key org-mode-map (kbd "C-c C-x s") 'org-download-screenshot)
  (define-key org-mode-map (kbd "C-c C-x y") 'org-download-yank))


(maybe-require-package 'crux)

(global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c R") #'crux-rename-buffer-and-file)
(global-set-key (kbd "C-c k") #'crux-kill-other-buffers)
(global-set-key (kbd "C-^") #'crux-top-join-line)

(provide 'init-local)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-local.el ends here
