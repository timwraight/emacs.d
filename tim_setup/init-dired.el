(require 'dired)
; truncate lines in dired mode
(add-hook 'dired-mode-hook 'toggle-truncate-lines)
(add-hook 'dired-mode-hook 'buffer-switch-to-monospaced)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)
(define-key dired-mode-map (kbd "o") 'switch-window)
(define-key dired-mode-map (kbd "M-g") 'revert-buffer)
(with-eval-after-load "evil"
	(evil-define-key 'normal dired-mode-map (kbd "M-g") 'revert-buffer)
	(evil-define-key 'normal dired-mode-map (kbd "w") 'wdired-change-to-wdired-mode)
    )
(setq dired-dwim-target t)
