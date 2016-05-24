;; SOLARIZED
(load-theme 'zenburn t)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(when window-system
  (set-face-attribute 'default nil :height 160)
  (set-face-attribute 'variable-pitch nil :height 190 :family "Helvetica")
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

(add-hook 'prog-mode-hook 'buffer-switch-to-monospaced)

;; ALERTS
(setq ring-bell-function 'ignore)

;; TYPOGRAPHY
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq truncate-lines t)
                            (rainbow-delimiters-mode)))
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode t)))

;; allow us to easily switch to monospaced or variable pitch fonts
;; per buffer. Need to define these faces, obviously
(defun buffer-switch-to-monospaced ()
  (interactive)
  (face-remap-add-relative 'default 'fixed-pitch)
  )

(defun buffer-switch-to-variable-pitch ()
  (interactive)
  (face-remap-add-relative 'default 'variable-pitch)
  )



;; Time format for modeline
(setq display-time-string-forms
      '(" " 24-hours ":" minutes
        (if time-zone " (") time-zone (if time-zone ")")))

;; MODE-LINE
(display-time)
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   "
               mode-line-position vimp-mode-line-tag
               (vc-mode vc-mode)
               "  " mode-line-misc-info mode-line-end-spaces))

; truncate lines in dired mode
(add-hook 'dired-mode-hook 'toggle-truncate-lines)


;; EDIFF
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)


;; DESKTOP
(setq desktop-path "~/.emacs.d/.emacs.desktop")
(desktop-save-mode 1)
