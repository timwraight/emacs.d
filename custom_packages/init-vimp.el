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
                           ;; interactive-haskell-mode
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
; Make RET do default Emacsy things instead of vim-movement

(defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))

(my-move-key vimp-motion-state-map vimp-normal-state-map (kbd "RET"))

(define-key vimp-normal-state-map (kbd "M-m") 'helm-proj)
(define-key vimp-normal-state-map (kbd "m") 'helm-timi)
(define-key vimp-insert-state-map (kbd "M-t") 'vimp-normal-state)
(define-key vimp-normal-state-map (kbd "M-b") 'helm-bible-search)
(global-set-key (kbd "M-t") 'vimp-normal-state)
(define-key vimp-normal-state-map (kbd "M-/") 'helm-occur)
(define-key vimp-normal-state-map (kbd "M-h") 'helm-swoop-without-pre-input)

(define-key vimp-normal-state-map (kbd "M-E") 'helm-resume)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)
(define-key vimp-normal-state-map (kbd "' m") 'helm-global-mark-ring)
(define-key vimp-normal-state-map (kbd "' k") 'helm-show-kill-ring)
(define-key vimp-normal-state-map (kbd "' x") 'helm-M-x)
(define-key vimp-normal-state-map (kbd "' f") 'helm-find-files)
(define-key vimp-normal-state-map (kbd "M-v") 'nil)
(define-key vimp-normal-state-map (kbd "M-v") 'lalopmak-vimp-paste-below-then-normal)
(define-key vimp-normal-state-map (kbd "SPC") vimp-leader--default-map)

(define-key vimp-insert-state-map (kbd "RET") 'electric-newline-and-maybe-indent)

(defun mirror-last-arg ()
    (interactive)
  (insert (concat "=" (word-at-point)))
  )
;;  keybinding already used by mail
;; (define-key vimp-insert-state-map (kbd "C-M-m") 'mirror-last-arg)


 


;; Make movement keys work like they should
(define-key vimp-normal-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-normal-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-next-line>") 'vimp-next-visual-line)
(define-key vimp-motion-state-map (kbd "<remap> <vimp-previous-line>") 'vimp-previous-visual-line)

;; Easiest way to do this, rather than put a hook on every keypress to
;; set that 'space was not pressed' is just to set a timestamp of last
;; time the key was pressed, and if it was within the last 0.5 seconds, insert a p


(setq last-space-insert (current-time))

(defun space-or-period ()
  (interactive)
  (if (and (< (time-to-seconds (time-subtract (current-time) last-space-insert)) 0.2)
           (string= " " (string (char-before (point)))))
           (progn
             (backward-char 1)
             (insert ".")
             (forward-char)
             )  
    (progn
      (insert " ")
      (setq last-space-insert (current-time))
      )
    )
  )  
   
 (vimp-define-key 'insert prog-mode-map (kbd "SPC") 'space-or-period)
      
(vimp-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
(with-eval-after-load "git-commit"
  (vimp-define-key 'insert with-editor-mode-map (kbd "M-k") 'git-kill-to-comments)
  )


;; Fix these functions so that they don't skip forwards so that you can use them
;; multiple times
(defun increment-number-at-point ()
      (interactive)
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
      (interactive)
      (or (looking-at "[0-9]+")
          (error "No number at point"))
      (replace-match (number-to-string (1- (string-to-number (match-string 0))))))


(define-key vimp-normal-state-map (kbd "C-M-u") 'increment-number-at-point)
(define-key vimp-normal-state-map (kbd "C-M-e") 'decrement-number-at-point)

; Use magit mode's default show commit command for RET
(vimp-define-key 'normal magit-blame-mode-map (kbd "RET") 'magit-show-commit)
(vimp-define-key 'normal magit-blame-mode-map (kbd "SPC") 'magit-diff-show-or-scroll-up)

(vimp-define-key 'normal git-rebase-mode-map (kbd "s") (lambda () (interactivet) (git-rebase-squash)))
(vimp-define-key 'normal git-rebase-mode-map (kbd "M-e") (lambda () (interactivet) (git-rebase-edit)))
(with-eval-after-load 'company
  (vimp-define-key 'insert company-active-map (kbd "M-m") 'company-filter-candidates))

(global-set-key (kbd "M-,") 'pop-global-mark)
(vimp-global-set-key 'normal (kbd "M-,") 'pop-global-mark)
