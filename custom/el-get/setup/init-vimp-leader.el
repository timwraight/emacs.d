; VIMP LEADER
(global-vimp-leader-mode)
(vimp-leader/set-leader ",")
(vimp-leader/set-key "=" 'balance-windows)
(vimp-leader/set-key "a" (lambda () (interactive)
                           (ack
                            (read-string "Search for: " (word-at-point))  ; search term
                            nil  ; interpret string as regex
                            (helm-ls-git-root-dir)  ; root directory
                                )))
(vimp-leader/set-key "b" 'ido-switch-buffer)
(vimp-leader/set-key "c" 'flyspell-auto-correct-previous-word)
(vimp-leader/set-key "d" 'sql-connect)
(vimp-leader/set-key "e" 'next-error)
(vimp-leader/set-key "f" 'recentf-ido-find-file)
(vimp-leader/set-key "g" 'magit-status)
(vimp-leader/set-key "h" 'vc-version-ediff)
(vimp-leader/set-key "l" 'split-window-below)
(vimp-leader/set-key "n" 'winring-new-configuration)
(vimp-leader/set-key "r" (lambda() (interactive) (kbd "ysiW")))
(vimp-leader/set-key "s" 'save-buffer)
(vimp-leader/set-key "t" 'google-this)
(vimp-leader/set-key ";" 'helm-mini)
(vimp-leader/set-key "w" 'previous-error)
(vimp-leader/set-key "z" (lambda () (interactive) (save-buffers-kill-terminal 1)))
(vimp-leader/set-key-for-mode 'sgml-mode "<right>" 'tagedit-forward-slurp-tag)
(vimp-leader/set-key-for-mode 'sgml-mode "<left>" 'tagedit-forward-barf-tag)
