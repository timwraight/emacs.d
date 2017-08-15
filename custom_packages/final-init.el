;; I had this defined in init-vimp.el, but it kept getting overwritten
(define-key evil-insert-state-map (kbd "M-i") 'sp-forward-sexp)
(define-key evil-insert-state-map (kbd "M-n") 'sp-backward-sexp)
(define-key evil-insert-state-map (kbd "M-y") 'sp-forward-slurp-sexp)
(define-key evil-insert-state-map (kbd "M-l") 'sp-forward-barf-sexp)
(define-key evil-insert-state-map (kbd "M-u") 'sp-up-sexp)
(define-key evil-insert-state-map (kbd "M-e") 'sp-down-sexp)
(define-key evil-insert-state-map (kbd "M-C-u") 'sp-backward-up-sexp)
(define-key evil-insert-state-map (kbd "M-C-e") 'sp-backward-down-sexp)


;; Oddly, this is an easier key mapping for my keyboard than just '$'
(define-key evil-visual-state-map (kbd "M-C-i") 'end-of-line)
(define-key evil-insert-state-map (kbd "M-C-i") 'end-of-line)
(define-key evil-normal-state-map (kbd "M-C-i") 'end-of-line)
(define-key evil-visual-state-map (kbd "M-C-n") 'evil-first-non-blank)
(define-key evil-insert-state-map (kbd "M-C-n") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "M-C-n") 'evil-first-non-blank)

;; (define-key evil-insert-state-map (kbd "M-z") (lambda () (interactive) (newline-and-indent) (previous-line) (indent-according-to-mode)))
(define-key evil-insert-state-map (kbd "M-z") 'open-line)
(define-key evil-insert-state-map (kbd "M-j") 'evil-join)
(define-key yas-minor-mode-map (kbd "M-t") 'yas-expand)
(define-key evil-normal-state-map (kbd "o") 'switch-window)


(define-key evil-normal-state-map (kbd "M-t")
  (lambda ()
    (interactive)
    (message "Making cursor at point...")
    (evil-mc-pause-cursors)
    (evil-mc-make-cursor-here)
    )
  )
(define-key evil-normal-state-map (kbd "M-r")
  (lambda ()
    (interactive)
    (if (evil-mc-frozen-p)
        (progn
          (message "Resuming cursors...")
          (evil-mc-resume-cursors)
          )
      (progn
        (message "Pausing cursors...")
        (evil-mc-pause-cursors)))))
(define-key evil-normal-state-map (kbd "M-d") 'evil-mc-undo-cursor-at-pos)
(add-hook 'evil-insert-state-entry-hook 'evil-mc-resume-cursors)
(add-hook 'evil-normal-state-entry-hook 'evil-mc-pause-cursors)
