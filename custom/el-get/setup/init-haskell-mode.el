(add-hook 'haskell-mode-hook 'inferior-haskell-mode)
(add-hook 'haskell-interactive-mode-hook 'vimp-insert-state)
(add-hook 'haskell-error-mode-hook 'vimp-insert-state)
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)


    (define-key haskell-interactive-mode-map (kbd "M-u") 'haskell-interactive-mode-history-previous)
    (define-key haskell-interactive-mode-map (kbd "M-e") 'haskell-interactive-mode-history-next)
    ))
