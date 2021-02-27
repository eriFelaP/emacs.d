;;; init-elpy.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://realpython.com/emacs-the-best-python-editor/
;; conda create -n emacs_elpy3 python=3 autopep8 virtualenv jedi rope yapf black flake8 jupyter
;; conda create -n emacs_elpy2 python=2 autopep8 virtualenv jedi rope yapf flake8 jupyter
;; conda install autopep8 virtualenv jedi rope yapf black flake8


(maybe-require-package 'elpy)
(maybe-require-package 'conda)

(conda-env-initialize-interactive-shells)

(custom-set-variables
 '(conda-anaconda-home "D:/ProgramData/Anaconda3"))
(setq conda-env-home-directory "D:/ProgramData/"
      conda-env-subdirectory "envs")


(setq elpy-rpc-virtualenv-path 'current)

(defun znh/set-flycheck-python-exe (name)
  "Confige elpy rep, NAME."
  (setq elpy-rpc-virtualenv-path conda-env-current-path)
  (setq python-shell-interpreter (expand-file-name "jupyter"
                                                   (expand-file-name "Scripts" conda-env-current-path))
        python-shell-interpreter-args  "console --simple-prompt")
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter")
  (setq elpy-rpc-python-command (expand-file-name "python" conda-env-current-path))
  (setq flycheck-python-pycompile-executable elpy-rpc-python-command))

(advice-add 'conda-env-activate :after 'znh/set-flycheck-python-exe)

(after-load 'elpy
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(elpy-enable)
(conda-env-activate "base")

(provide 'init-elpy)
;;; init-elpy.el ends here
