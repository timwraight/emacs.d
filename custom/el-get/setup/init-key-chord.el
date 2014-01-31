(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.1)


;; GLOBAL (windows and buffers)
(key-chord-define-global ",b" 'ido-switch-buffer)
(key-chord-define-global ",r" 'recentf-ido-find-file)
(key-chord-define-global ",k" 'kill-this-buffer)
(key-chord-define-global ",1" 'delete-other-windows)
(key-chord-define-global ",0" 'delete-window)
(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",x" 'smex)
(key-chord-define-global ",p" 'split-window-horizontally)
(key-chord-define-global ",v" 'clipboard-yank)
(key-chord-define-global ",;" 'split-window-vertically)
(key-chord-define-global ";;" 'helm-mini)


;; GLOBAL (org)
(key-chord-define-global ",l" 'org-store-link)

;; GLOBAL (editing)
(key-chord-define-global ",c" 'flyspell-auto-correct-previous-word)
(key-chord-define-global ",d" 'backward-kill-word)
