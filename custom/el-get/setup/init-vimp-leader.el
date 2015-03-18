; VIMP LEADER
(global-vimp-leader-mode)
(vimp-leader/set-leader "s")

(vimp-leader/set-key "'" (kbd "yse'"))
(vimp-leader/set-key ";" 'helm-mini)
(vimp-leader/set-key "=" 'balance-windows)
(vimp-leader/set-key "\"" (kbd "yse\""))
(vimp-leader/set-key "a" 'helm-git-grep-at-point)
(vimp-leader/set-key "d" 'sql-connect)
(vimp-leader/set-key "e" 'next-error)
(vimp-leader/set-key "f" 'helm-mu)
(vimp-leader/set-key "g" 'magit-status)
(vimp-leader/set-key "h" 'vc-version-ediff)
(vimp-leader/set-key "l" 'split-window-below)
(vimp-leader/set-key "o" 'helm-org-agenda-files-headings)
(vimp-leader/set-key "r" (lambda() (interactive) (kbd "ysiW")))
(vimp-leader/set-key "t" 'google-this)
(vimp-leader/set-key "w" 'previous-error)
(vimp-leader/set-key "z" (lambda () (interactive) (save-buffers-kill-terminal 1)))


; copied from key-chord-mode

(vimp-leader/set-key "b" 'ido-switch-buffer)
;; GLOBAL (windows and buffers)
(vimp-leader/set-key "r" 'helm-recentf)
(vimp-leader/set-key "k" 'kill-this-buffer)
(vimp-leader/set-key "1" 'delete-other-windows)
(vimp-leader/set-key "0" 'delete-window)
(vimp-leader/set-key "s" 'save-buffer)
(vimp-leader/set-key "m" 'helm-jabber-contacts)
(vimp-leader/set-key "x" 'helm-M-x)
(vimp-leader/set-key "v" 'clipboard-yank)
(vimp-leader/set-key ";" 'split-window-vertically)
(vimp-leader/set-key ";" 'helm-mini)

(vimp-leader/set-key "i" 'forward-symbol)
(vimp-leader/set-key "n" (lambda () (interactive) (forward-symbol -1)))

;; AUTO-COMPLETE
;; Use kj to escape from auto-complete mode
;(key-chord-define ac-menu-map "lk" 'ac-stop)
;(key-chord-define ac-completing-map "lk" 'ac-stop)
;(key-chord-define ac-menu-map "ff" 'ac-isearch)

;; GLOBAL (org)
(vimp-leader/set-key "l" 'org-store-link)

;; GLOBAL (editing)
(vimp-leader/set-key "c" 'flyspell-auto-correct-previous-word)


(vimp-leader/set-key-for-mode 'sgml-mode "<right>" 'tagedit-forward-slurp-tag)
(vimp-leader/set-key-for-mode 'sgml-mode "<left>" 'tagedit-forward-barf-tag)
