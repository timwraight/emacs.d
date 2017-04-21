(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line
(load-file "~/.emacs.d/package_loader.el")

(setq custom-file "~/.emacs.d/tim_setup/setup-specific.el")
(load "~/.emacs.d/tim_setup/setup-specific.el")
(put 'erase-buffer 'disabled nil)
