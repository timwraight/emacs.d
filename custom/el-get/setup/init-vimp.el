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
                           magit-blame-mode
                           magit-diff-mode
                           picture-mode
                           fundamental-mode
                           undo-tree-visualizer-mode
                           project-explorer-mode))
  (vimp-set-initial-state mode 'emacs))

(dolist (mode '(jabber-chat-mode
                eshell-mode
                erc-mode
                message-mode
                git-commit-mode
                mu4e-compose-mode
                ))
  (vimp-set-initial-state mode 'insert))


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
(global-set-key (kbd "M-t") 'vimp-normal-state)
(define-key vimp-normal-state-map (kbd "M-/") 'helm-occur)
(define-key vimp-normal-state-map (kbd "M-E") 'helm-resume)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)
;; Make movement keys work like they should
(define-key vimp-normal-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-normal-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)
