
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.2)
(setq key-chord-one-key-delay 0)


;; GLOBAL
(key-chord-define-global ",b" 'ido-switch-buffer)
(key-chord-define-global ",r" 'recentf-ido-find-file)
(key-chord-define-global ",f" 'ido-find-file)
(key-chord-define-global ",x" 'delete-window)
(key-chord-define-global ",1" 'delete-other-windows)
(key-chord-define-global ",s" 'save-buffer)

;; GLOBAL (editing)
(key-chord-define-global ",c" 'flyspell-auto-correct-previous-word)
