(require 'magit)
(require 'git-commit-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'magit-log-edit-mode-hook 'vimp-insert-state)

; lalopmak bindings for status dashboard
(define-key magit-status-mode-map "e" 'next-line)
(define-key magit-status-mode-map "u" 'previous-line)
(define-key magit-status-mode-map "n" 'magit-unstage-item)
(define-key magit-status-mode-map "i" 'magit-stage-item)

; lalopmak bindings for diff
(define-key magit-diff-mode-map "e" 'next-line)
(define-key magit-diff-mode-map "u" 'previous-line)
(define-key magit-diff-mode-map "n" 'magit-unstage-item)
(define-key magit-diff-mode-map "i" 'magit-ediff)
