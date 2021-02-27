;;; init-elpy.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://realpython.com/emacs-the-best-python-editor/

(maybe-require-package 'elpy)
(maybe-require-package 'pyvenv)
(setenv "WORKON_HOME" (getenv "CONDA_ENVS_PATH"))
;; conda create -n emacs_elpy3 python=3 autopep8 virtualenv jedi rope yapf black flake8 jupyter
;; conda create -n emacs_elpy2 python=2 autopep8 virtualenv jedi rope yapf flake8 jupyter
;; conda install autopep8 virtualenv jedi rope yapf black flake8

(setq elpy-rpc-virtualenv-path 'current)

(defun znh/set-flycheck-python-exe (name)
  "Confige elpy rep, NAME."
  (setq elpy-rpc-virtualenv-path
        (expand-file-name name (getenv "CONDA_ENVS_PATH")))
  (setq elpy-rpc-python-command (expand-file-name "python" elpy-rpc-virtualenv-path))
  (setq flycheck-python-pycompile-executable elpy-rpc-python-command))

(advice-add 'pyvenv-workon :after 'znh/set-flycheck-python-exe)

(after-load 'elpy
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(elpy-enable)
(pyvenv-mode 1)
(pyvenv-workon "emacs_elpy3")

(provide 'init-elpy)
;;; init-elpy.el ends here
