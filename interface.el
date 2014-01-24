;; SOLARIZED
(load-theme 'zenburn t)
(menu-bar-mode -1)

(global-hl-line-mode)
(column-number-mode 1)
(setq max-mini-window-height 1)
(setq inhibit-splash-screen t)
(auto-compression-mode t)
(global-font-lock-mode t)
(show-paren-mode 1)
(setq-default line-spacing 0.2)

;; default to unified diffs
(setq diff-switches "-u")

(global-rainbow-delimiters-mode)

;; ALERTS
(setq ring-bell-function 'ignore)

;; TYPOGRAPHY
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq fill-column 79)
                            (toggle-truncate-lines)))
(global-visual-line-mode t)

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
    'org-mode-line-string
    " --"
    "%-" ;; fill with '-'
    ))


; truncate lines in dired mode
(add-hook 'dired-mode-hook 'toggle-truncate-lines)


;; EDIFF
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
