;; VIMP

(vimp-mode 1)

(dolist (mode '(eshell-mode shell-mode term-mode terminal-mode comint-mode skewer-repl-mode
                profiler-report-mode
                erc-mode weechat-mode
                direx:direx-mode
                project-explorer-mode))
  (vimp-set-initial-state mode 'emacs))


(setq vimp-auto-indent nil)
(global-set-key (kbd "<f1>") 'vimp-local-mode)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define vimp-insert-state-map "kj" 'vimp-normal-state)
; Make RET and SPACE do default Emacsy things instead of vim-movement

(defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil)
    )
  (my-move-key vimp-motion-state-map vimp-normal-state-map (kbd "RET"))
  (my-move-key vimp-motion-state-map vimp-normal-state-map " ")

(define-key vimp-normal-state-map "m" 'helm-ls-git-ls)
(define-key vimp-normal-state-map ";" 'helm-mini)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)
