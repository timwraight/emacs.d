(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)
; (add-hook 'magit-status-mode-map (lambda () (yas-minor-mode t))) 

; lalopmak bindings for status dashboard
(define-key magit-status-mode-map "e" 'next-line)
(define-key magit-status-mode-map "u" 'previous-line)
; (define-key magit-file-section-map "u" 'previous-line)
;(define-key magit-hunk-section-map "u" 'previous-line)
;(define-key magit-unstaged-section-map "u" 'previous-line)
;(define-key magit-unstaged-section-map "i" 'magit-stage)
;(define-key magit-staged-section-map "u" 'previous-line)
(define-key magit-status-mode-map "n" 'magit-unstage-item)
;(define-key magit-staged-section-map "n" 'magit-unstage-all)
(define-key magit-status-mode-map "i" 'magit-stage-item)
(define-key magit-status-mode-map (kbd "m") 'helm-timi)
; (define-key magit-file-section-map "<TAB>" 'magit-section-toggle)
;(define-key magit-staged-section-map "<TAB>" 'magit-section-toggle)
;(define-key magit-unstaged-section-map "<TAB>" 'magit-section-toggle)

; lalopmak bindings for diff
(define-key magit-diff-mode-map "e" 'next-line)
(define-key magit-diff-mode-map "u" 'previous-line)
(define-key magit-diff-mode-map "n" 'magit-unstage-file)
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
