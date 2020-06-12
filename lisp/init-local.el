;;; init-local.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

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
                  "当行则行，当止则止。"
                  )))
    (nth (random (length mottos))
         mottos)))

(setq initial-scratch-message
      (concat ";; " (znh/motto-of-the-day)))


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
(add-hook 'after-make-window-system-frame-hooks
          (lambda ()
            (set-face-attribute 'default nil :font "Noto Sans Mono 10")
            (dolist (charset '(kana han cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font) charset
                                (font-spec :family "Noto Sans CJK SC Regular")))
            (setq face-font-rescale-alist '(("Noto Sans Mono" . 1)
                                            ("Noto Sans CJK SC Regular" . 1.2)))))

;; 使用中文的格式
(setq system-time-locale "C")

;;----------------------------------------------------------------------
;; 把 C-SPC 留个系统输入法切换；把 Emacs 内置输入法切换换成设置标记。
;;----------------------------------------------------------------------
(global-unset-key (kbd "C-SPC"))

(global-set-key (kbd "C-\\") #'set-mark-command)
(global-set-key (kbd "C-x r C-\\") #'point-to-register)

;;----------------------------------------------------------------------
;; 配置一些常用的全局快捷键。
;;----------------------------------------------------------------------
(global-set-key (kbd "C-x \\") #'align-regexp)
(global-set-key (kbd "M-o") #'switch-window)
(global-set-key (kbd "M-g c") #'avy-goto-char)
(global-set-key (kbd "C-c j") #'avy-goto-char)

(defalias 'rc 'revert-buffer-with-coding-system)
(defalias 'sc 'set-buffer-file-coding-system)
(defalias 'ds 'desktop-save)
(defalias 'dc 'desktop-change-dir)

(maybe-require-package 'easy-kill)

(global-set-key (kbd "M-\\") 'easy-mark)


(maybe-require-package 'crux)

(global-set-key (kbd "C-c D") #'crux-delete-file-and-buffer)
(global-set-key (kbd "C-c R") #'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c k") #'crux-kill-other-buffers)
(global-set-key (kbd "C-x C-r") #'crux-recentf-find-file)
(global-set-key (kbd "C-^") #'crux-top-join-line)
(global-set-key (kbd "C-c C-r") #'crux-recentf-find-file)

(setq custom-enabled-themes '(sanityinc-tomorrow-day))

(defun znh/copy-file-name-to-clipboard (&optional full-path-p)
  "Copy the current buffer file name to the clipboard.

If FULL-PATH-P is TRUE, copy the full path.

https://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
http://ergoemacs.org/emacs/emacs_copy_file_path.html"
  (interactive "P")
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (if full-path-p
          (progn
            (kill-new filename)
            (message "Copied buffer file name '%s' to the clipboard." filename))
        (kill-new (file-name-nondirectory filename))
        (message "Copied buffer file name '%s' to the clipboard."
                 (file-name-nondirectory filename))))))

(auto-save-visited-mode)

(setq auto-save-visited-interval 180)

(maybe-require-package "deft")


(require 'paredit)

(define-key paredit-mode-map (kbd "C-c )") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-c (") 'paredit-forward-barf-sexp)

;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun xah-open-in-external-app (&optional @fname)
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.

When called in emacs lisp, if @fname is given, open that.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
  (interactive)
  (let* (
         ($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath)) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                        (start-process "" nil "xdg-open" $fpath))) $file-list))))))

(global-set-key [remap whole-line-or-region-kill-ring-save] 'easy-kill)


(when (and (eq system-type 'windows-nt)
           (eq w32-ansi-code-page 65001))
  (setq w32-system-coding-system 'utf-8)
  (define-coding-system-alias 'cp65001 'utf-8))


(provide 'init-local)



;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-local.el ends here
