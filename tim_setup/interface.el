;; KEYS
(setq mac-right-option-key-is-meta nil
      mac-right-command-key-is-meta t
      mac-right-command-modifier 'meta
      mac-right-option-modifier 'none)

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
(setq desktop-path '("~/.emacs.d/.emacs.desktop"))
(desktop-save-mode 1)



;; SAVED FACES FROM 'customize'
;; I can't find an easy way of saving them as normal emacs lisp, so am just saving
;; them here commented, so that I don't lose them

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#222" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "Helvetica"))))
;;  '(company-scrollbar-fg ((t (:background "goldenrod1"))))
;;  '(elscreen-tab-background-face ((t (:background "gray13"))))
;;  '(elscreen-tab-current-screen-face ((t (:inherit fixed-pitch :background "DarkGoldenrod3" :foreground "white"))))
;;  '(elscreen-tab-other-screen-face ((t (:inherit fixed-pitch :background "CadetBlue4" :foreground "white"))))
;;  '(fixed-pitch ((t (:height 150 :family "Consolas"))))
;;  '(flycheck-color-mode-line-error-face ((t (:inherit flycheck-fringe-error :weight normal))))
;;  '(flycheck-color-mode-line-warning-face ((t (:inherit flycheck-fringe-warning :weight normal))))
;;  '(helm-ff-directory ((t nil)))
;;  '(helm-ff-executable ((t nil)))
;;  '(hl-line ((t (:inherit highlight :background "#313131"))))
;;  '(italic ((t (:foreground "#aaa" :slant italic))))
;;  '(magit-diff-added ((t (:foreground "DarkOliveGreen4"))))
;;  '(magit-diff-added-highlight ((t (:foreground "DarkOliveGreen3"))))
;;  '(magit-diff-removed ((t (:foreground "red4"))))
;;  '(magit-diff-removed-highlight ((t (:foreground "red3"))))
;;  '(mode-line ((t (:background "#1C1B19" :foreground "#8FB28F" :box (:line-width -1 :style released-button)))))
;;  '(mode-line-inactive ((t (:inherit mode-line :background "#000" :foreground "#191C19" :box (:line-width -1 :style released-button) :weight light))))
;;  '(mu4e-header-face ((t (:inherit fixed-pitch))))
;;  '(mu4e-header-highlight-face ((t (:inherit fixed-pitch :background "dark cyan" :weight normal :height 150))))
;;  '(mu4e-header-marks-face ((t (:inherit font-lock-preprocessor-face :family "Consolas"))))
;;  '(mu4e-highlight-face ((t (:inherit fixed-pitch))))
;;  '(mu4e-replied-face ((t (:inherit fixed-pitch :foreground "#6F6F6F"))))
;;  '(mu4e-unread-face ((t (:foreground "white" :family "Consolas"))))
;;  '(org-special-keyword ((t (:inherit font-lock-keyword-face :foreground "#656C5F" :weight normal))))
;;  '(popup-isearch-match ((t (:background "DarkGoldenrod2" :foreground "#DCDCCC"))))
;;  '(region ((t (:background "#404013"))))
;;  '(undo-tree-visualizer-active-branch-face ((t (:foreground "yellow"))))
;;  '(variable-pitch ((t (:height 160 :family "Helvetica")))))
