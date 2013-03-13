;; PACKAGES


(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))


(add-to-list 'load-path "~/.emacs.d/vendor/")
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))


;; SOLARIZED 

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)

(require 'vimp )

;; PYTHON
(add-to-list 'load-path "~/.emacs.d/vendor/python-mode/")
(setq py-install-directory "~/.emacs.d/vendor/python-mode/")
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py$" . python2-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
