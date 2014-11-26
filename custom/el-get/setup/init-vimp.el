;; VIMP


(vimp-mode 1)

(dolist (mode '(eshell-mode shell-mode term-mode terminal-mode comint-mode skewer-repl-mode
                profiler-report-mode
                erc-mode weechat-mode
                direx:direx-mode
                picture-mode
                project-explorer-mode))
  (vimp-set-initial-state mode 'emacs))

(dolist (mode '(git-commit-mode))
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

(define-key vimp-normal-state-map "m" 'helm-ls-git-ls)
(define-key vimp-normal-state-map ";" 'helm-mini)
(define-key vimp-normal-state-map (kbd "M-/") 'helm-occur)
(define-key vimp-normal-state-map (kbd "M-E") 'helm-resume)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)
;; Make movement keys work like they should
(define-key vimp-normal-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-normal-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)
