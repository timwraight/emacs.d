(add-hook 'haskell-mode-hook 'structured-haskell-mode)
(add-hook 'haskell-mode-hook
          (lambda () (interactive)
              (autopair-mode -1)))
(set-face-background 'shm-quarantine-face "#4d424d")
(set-face-background 'shm-current-face "#826f82")
