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
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js-mode-hook 'rainbow-delimiters-mode)

;; TYPOGRAPHY

(setq line-spacing 4)
(global-visual-line-mode t)


;; SOLARIZED 

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)



;; MODE-LINE

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    "%b "
    ;; line and column
    "(%02l, %02c)"
    ;; the current major mode for the buffer.
    "[%m] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    'org-mode-line-string
    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    ))


(global-set-key (kbd "C-S-M-k") '(lambda ()
				   (interactive)
				   (enlarge-window 1)))

(global-set-key (kbd "C-S-M-j") '(lambda ()
				   (interactive)
				   (enlarge-window -1)))
