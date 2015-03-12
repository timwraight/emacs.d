(require 'magit)
(require 'git-commit-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'magit-log-edit-mode-hook 'vimp-insert-state)

; lalopmak bindings for status dashboard
(define-key magit-status-mode-map "e" 'next-line)
(define-key magit-status-mode-map "u" 'previous-line)
(define-key magit-status-mode-map "n" 'magit-unstage-item)
(define-key magit-status-mode-map "i" 'magit-stage-item)
(define-key magit-status-mode-map (kbd "m") 'helm-timi)

; lalopmak bindings for diff
(define-key magit-diff-mode-map "e" 'next-line)
(define-key magit-diff-mode-map "u" 'previous-line)
(define-key magit-diff-mode-map "n" 'magit-unstage-item)
(define-key magit-diff-mode-map "i" 'magit-ediff)

;; full screen magit-status
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
