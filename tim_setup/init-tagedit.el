(require 'tagedit)
(define-key html-mode-map (kbd "M-i") 'tagedit-forward-slurp-tag)
(define-key html-mode-map (kbd "M-n") 'tagedit-forward-barf-tag)
(tagedit-add-experimental-features)
(add-hook 'sgml-mode-hook (lambda () (interactive) (tagedit-mode 1)))


