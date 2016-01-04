;; VIMP

(require 'vimp)
(vimp-mode 1)

(dolist (mode '(shell-mode term-mode terminal-mode comint-mode skewer-repl-mode
                           profiler-report-mode
                           weechat-mode
                           direx:direx-mode
                           magit-mode
                           magit-commit-popup-mode
                           magit-modes
                           magit-popup-mode
                           git-timemachine-mode
                           magit-blame-mode
                           interactive-haskell-mode
                           haskell-error-mode
                           magit-diff-mode
                           picture-mode
                           undo-tree-visualizer-mode
                           project-explorer-mode))
  (vimp-set-initial-state mode 'emacs))

(dolist (mode '(jabber-chat-mode
                eshell-mode
                erc-mode
                message-mode
                global-git-commit-mode
                mu4e-compose-mode
                ))
  (vimp-set-initial-state mode 'insert))

(dolist (mode '(text-mode
                prog-mode
                ))
  (vimp-set-initial-state mode 'normal))


; magic searches
(setq vimp-magic 'very-magic)


(setq vimp-auto-indent nil)
(global-set-key (kbd "<f1>") 'vimp-local-mode)
; Make RET and SPACE do default Emacsy things instead of vim-movement

(defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))

(my-move-key vimp-motion-state-map vimp-normal-state-map (kbd "RET"))
(my-move-key vimp-motion-state-map vimp-normal-state-map " ")

(define-key vimp-normal-state-map (kbd "M-m") 'helm-proj)
(define-key vimp-normal-state-map (kbd "m") 'helm-timi)
(define-key vimp-insert-state-map (kbd "M-t") 'vimp-normal-state)
(define-key vimp-normal-state-map (kbd "M-b") 'helm-bible-search)
(define-key vimp-insert-state-map (kbd "M-e") 'yas-expand)
(global-set-key (kbd "M-t") 'vimp-normal-state)
(define-key vimp-normal-state-map (kbd "M-/") 'helm-occur)
(define-key vimp-normal-state-map (kbd "M-h") 'helm-swoop)
(define-key vimp-normal-state-map (kbd "M-h") 'helm-swoop)
(define-key vimp-normal-state-map (kbd "M-.") 'next-buffer)
(define-key vimp-normal-state-map (kbd "M-,") 'previous-buffer)

(define-key vimp-normal-state-map (kbd "M-E") 'helm-resume)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)
(define-key vimp-normal-state-map (kbd "' m") 'helm-global-mark-ring)
(define-key vimp-normal-state-map (kbd "' k") 'helm-show-kill-ring)
(define-key vimp-normal-state-map (kbd "' x") 'helm-M-x)


;; Make movement keys work like they should
(define-key vimp-normal-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-normal-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)
