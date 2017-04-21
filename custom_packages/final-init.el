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
(define-key evil-insert-state-map (kbd "M-z") 'evil-open-above)
(define-key evil-insert-state-map (kbd "M-j") 'evil-join)
(define-key yas-minor-mode-map (kbd "M-t") 'yas-expand)
(define-key evil-normal-state-map (kbd "o") 'switch-window)
