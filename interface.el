;; SOLARIZED
(load-theme 'zenburn t)
(menu-bar-mode -1)

(when window-system
  (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'variable-pitch nil :height 190 :family "Calibri")
  (set-frame-parameter nil 'fullscreen 'fullboth)
  (add-hook 'mu4e-headers-mode-hook 'variable-pitch-mode)
  (server-start))

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


;; ALERTS
(setq ring-bell-function 'ignore)

;; TYPOGRAPHY
(setq fill-column 79)
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq truncate-lines t)
                            (visual-line-mode t)))
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode t)))
(global-visual-line-mode t)

;; Time format for modeline
(setq display-time-string-forms
      '(" " 24-hours ":" minutes
        (if time-zone " (") time-zone (if time-zone ")")))

;; MODE-LINE
(display-time)
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    "%b "
    'vc-mode
    " --"
    'jabber-activity-mode-string
    " --"
    'org-mode-line-string
    " --"
    'display-time-string
    "%-" ;; fill with '-'
    ))


; truncate lines in dired mode
(add-hook 'dired-mode-hook 'toggle-truncate-lines)


;; EDIFF
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
