;; INTERFACE
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(setq max-mini-window-height 1)
(setq inhibit-splash-screen t)
(tooltip-mode -1)
(auto-compression-mode t)
(global-font-lock-mode t)
(show-paren-mode 1)
;; default to unified diffs
(setq diff-switches "-u")

; Use C-x C-m for M-x
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)

;; TYPOGRAPHY

(setq line-spacing 4)
(global-visual-line-mode t)

;; Turn on auto-fill (ie, break lines after 80 chars or whatever) globally
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; SOLARIZED 

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)

