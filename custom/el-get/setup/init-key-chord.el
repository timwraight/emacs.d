(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.1)
(setq key-chord-one-key-delay 0.1)


(key-chord-define-global ",b" 'ido-switch-buffer)
;; GLOBAL (windows and buffers)
(key-chord-define-global ",r" 'helm-recentf)
(key-chord-define-global ",k" 'kill-this-buffer)
(key-chord-define-global ",1" 'delete-other-windows)
(key-chord-define-global ",0" 'delete-window)
(key-chord-define-global ",s" 'save-buffer)
(key-chord-define-global ",x" 'helm-M-x)
(key-chord-define-global ",i" 'split-window-horizontally)
(key-chord-define-global ",v" 'clipboard-yank)
(key-chord-define-global ",;" 'split-window-vertically)
(key-chord-define-global ";;" 'helm-mini)

;; AUTO-COMPLETE
;; Use kj to escape from auto-complete mode
;(key-chord-define ac-menu-map "lk" 'ac-stop)
;(key-chord-define ac-completing-map "lk" 'ac-stop)
;(key-chord-define ac-menu-map "ff" 'ac-isearch)

;; Text mode
(key-chord-define text-mode-map "  " '(kbd ". "))


;; GLOBAL (org)
(key-chord-define-global ",l" 'org-store-link)

;; GLOBAL (editing)
(key-chord-define-global ",c" 'flyspell-auto-correct-previous-word)
(key-chord-define-global ",d" 'backward-kill-word)
