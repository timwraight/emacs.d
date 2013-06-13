;; PACKAGES


(require 'package)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/vendor/")
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))


;; EL-GET

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)



;(setq dim-packages (append
;                    '(flycheck flycheck-color-mode-line)))
;(el-get 'sync dim-packages)

;; SOLARIZED 

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)


(require 'vimp )
;; VIMP LEADER
(require 'vimp-leader)
