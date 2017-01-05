;; I had this defined in init-vimp.el, but it kept getting overwritten
(define-key vimp-insert-state-map (kbd "M-i") 'sp-forward-sexp)
(define-key vimp-insert-state-map (kbd "M-n") 'sp-backward-sexp)
(define-key vimp-insert-state-map (kbd "M-y") 'sp-forward-slurp-sexp)
(define-key vimp-insert-state-map (kbd "M-l") 'sp-forward-barf-sexp)
(define-key vimp-insert-state-map (kbd "M-u") 'sp-up-sexp)
(define-key vimp-insert-state-map (kbd "M-e") 'sp-down-sexp)
(define-key vimp-insert-state-map (kbd "M-C-u") 'sp-backward-up-sexp)
(define-key vimp-insert-state-map (kbd "M-C-e") 'sp-backward-down-sexp)


;; Oddly, this is an easier key mapping for my keyboard than just '$'
(define-key vimp-visual-state-map (kbd "M-C-i") 'end-of-line)
(define-key vimp-insert-state-map (kbd "M-C-i") 'end-of-line)
(define-key vimp-normal-state-map (kbd "M-C-i") 'end-of-line)
(define-key vimp-visual-state-map (kbd "M-C-n") 'vimp-first-non-blank)
(define-key vimp-insert-state-map (kbd "M-C-n") 'vimp-first-non-blank)
(define-key vimp-normal-state-map (kbd "M-C-n") 'vimp-first-non-blank)

(define-key vimp-insert-state-map (kbd "M-z") (lambda () (interactive) (newline-and-indent) (previous-line) (indent-according-to-mode)))
(define-key vimp-insert-state-map (kbd "M-j") 'vimp-join)
(define-key yas-minor-mode-map (kbd "M-t") 'yas-expand)
(define-key vimp-normal-state-map (kbd "o") 'switch-window)
