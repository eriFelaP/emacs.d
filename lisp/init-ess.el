;;; init-ess.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(maybe-require-package 'ess)
(setq ess-use-flymake nil)

(with-eval-after-load "ess-r-mode"
  (define-key ess-r-mode-map "_" #'ess-insert-assign)
  (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign))

(provide 'init-ess)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ess.el ends here
