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
(setq-default line-spacing 0.2)

;; default to unified diffs
(setq diff-switches "-u")

; Use C-x C-m for M-x
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

(require 'rainbow-delimiters)
(add-hook 'python-mode-hook 'rainbow-delimiters-mode)
(add-hook 'js-mode-hook 'rainbow-delimiters-mode)

;; TYPOGRAPHY

; Line Spacing 
(setq line-spacing 4)
(add-hook 'prog-mode-hook (lambda ()
                            (interactive)
                            (setq line-spacing 2)))

(global-visual-line-mode t)
(require 'pretty-mode)
(global-pretty-mode 0)


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
    'org-mode-line-string
    " --"
    "%-" ;; fill with '-'
    ))


(global-set-key (kbd "C-S-M-k") '(lambda ()
				   (interactive)
				   (enlarge-window 1)))

(global-set-key (kbd "C-S-M-j") '(lambda ()
				   (interactive)
				   (enlarge-window -1)))

(global-set-key (kbd "C-S-M-l") '(lambda ()
				   (interactive)
				   (enlarge-window 1 1)))

(global-set-key (kbd "C-S-M-h") '(lambda ()
				   (interactive)
				   (enlarge-window -1 1)))

; Don't truncate lines when we're in dired mode
(add-hook 'dired-mode-hook 'toggle-truncate-lines)

;; BUFFER-MOVE

(load-file "~/.emacs.d/vendor/buffer-move.el")
(require 'buffer-move)
(global-set-key (kbd "C-S-s-k")     'buf-move-up)
(global-set-key (kbd "C-S-s-j")   'buf-move-down)
(global-set-key (kbd "C-S-s-h")   'buf-move-left)
(global-set-key (kbd "C-S-s-l")  'buf-move-right)

;; EDIFF

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(eval-after-load "vc-hooks"
         '(define-key vc-prefix-map "=" 'ediff-revision))

(add-hook 'ediff-load-hook
          (lambda ()

            (add-hook 'ediff-before-setup-hook
                      (lambda ()
                        (setq ediff-saved-window-configuration (current-window-configuration))))
            
            (let ((restore-window-configuration
                   (lambda ()
                     (set-window-configuration ediff-saved-window-configuration))))
              (add-hook 'ediff-quit-hook restore-window-configuration 'append)
              (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))
