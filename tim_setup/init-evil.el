;; EVIL

(require 'evil)
(evil-mode 1)

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
  (evil-set-initial-state mode 'emacs))

(dolist (mode '(jabber-chat-mode
                eshell-mode
                erc-mode
                message-mode
                global-git-commit-mode
                mu4e-compose-mode
                ))
  (evil-set-initial-state mode 'insert))

(dolist (mode '(text-mode
                prog-mode
                ))
  (evil-set-initial-state mode 'normal))


; magic searches
(setq evil-magic 'very-magic)

(setq evil-normal-state-cursor '(box "OrangeRed3"))
(setq evil-insert-state-cursor '((bar . 2)  "OrangeRed3"))


(setq evil-auto-indent nil)
(global-set-key (kbd "<f1>") 'evil-local-mode)
; Make RET do default Emacsy things instead of vim-movement

(defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil))

(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))

(setq tw/movement-key-side 'right)

(defun tw/toggle-movement-key-side ()
  "Toggle which side the movement keys are on (to try and reduce hand fatigue"
  (interactive)
  (case tw/movement-key-side
    ('right 
    ;;; Up/down/left/right
     (set-in-all-evil-states-but-insert "f" 'evil-previous-line)
     (set-in-all-evil-states-but-insert "s" 'evil-next-line)
     (set-in-all-evil-states-but-insert "r" 'evil-backward-char)
     (set-in-all-evil-states-but-insert "t" 'evil-forward-char)
    ;;; Beginning/end of line (home/end)
     (set-in-all-evil-states-but-insert "p" 'evil-insert)
     (set-in-all-evil-states-but-insert "\M-p" 'evil-append)
     (set-in-all-evil-states-but-insert "P" 'evil-insert-line)
     (set-in-all-evil-states-but-insert "\M-P" 'evil-append-line)
     (set-in-all-evil-states-but-insert "\M-f" 'lalopmak-evil-scroll-page-up)
     (set-in-all-evil-states-but-insert "\M-s" 'lalopmak-evil-scroll-page-down)
     (set-in-all-evil-states-but-insert "\M-t" 'evil-forward-little-word-end)
     (set-in-all-evil-states-but-insert "\M-T" 'forward-symbol)
     (set-in-all-evil-states-but-insert "\M-r" 'evil-backward-little-word-begin)
     (set-in-all-evil-states-but-insert "\M-R" (lambda () (interactive) (forward-symbol -1)))
     (set-in-all-evil-states-but-insert "u" 'helm-semantic-or-imenu)
     (setq mac-left-option-key-is-meta nil)
     (setq mac-left-option-modifier 'meta)
     (setq mac-right-option-modifier 'meta)
     (setq tw/movement-key-side 'left)
     (message "Set movement keys to left hand side")
     )

    ('left
    (set-in-all-evil-states-but-insert "u" 'evil-previous-line)
    (set-in-all-evil-states-but-insert "e" 'evil-next-line)
    (set-in-all-evil-states-but-insert "n" 'evil-backward-char)
    (set-in-all-evil-states-but-insert "i" 'evil-forward-char)
    ;;; Beginning/end of line (home/end)
    (set-in-all-evil-states-but-insert "y" 'evil-insert)
    (set-in-all-evil-states-but-insert "\M-y" 'evil-append)
    (set-in-all-evil-states-but-insert "Y" 'evil-insert-line)
    (set-in-all-evil-states-but-insert "\M-Y" 'evil-append-line)
    (set-in-all-evil-states-but-insert "\M-u" 'lalopmak-evil-scroll-page-up)
    (set-in-all-evil-states-but-insert "\M-e" 'lalopmak-evil-scroll-page-down)
    (set-in-all-evil-states-but-insert "\M-i" 'evil-forward-little-word-end)
    (set-in-all-evil-states-but-insert "\M-I" 'forward-symbol)
    (set-in-all-evil-states-but-insert "\M-n" 'evil-backward-little-word-begin)
    (set-in-all-evil-states-but-insert "\M-N" (lambda () (interactive) (forward-symbol -1)))
    (set-in-all-evil-states-but-insert "f" 'helm-semantic-or-imenu)
    (setq tw/movement-key-side 'right)
    (message "Set movement keys to right hand side")
    )
    )) 


(require 'evil-avy)


(define-key evil-normal-state-map (kbd "M-m") 'tw/git-files)
(define-key evil-normal-state-map (kbd "m") 'tw/helm-ag)
(define-key evil-normal-state-map (kbd "o") 'avy-goto-line)
(define-key evil-normal-state-map (kbd "w") 'avy-goto-char-in-line)
(define-key evil-normal-state-map (kbd "M-h") 'helm-swoop-without-pre-input)
(define-key evil-normal-state-map (kbd "M-f") 'tw/helm-gtags-dwim)
(define-key evil-insert-state-map (kbd "M-t") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "l") 'avy-goto-char-timer)
(define-key evil-normal-state-map (kbd "M-l") 'avy-goto-line)
(define-key evil-normal-state-map (kbd "M-b") 'helm-bible-search)
(global-set-key (kbd "M-t") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "M-/") 'helm-swoop-without-pre-input)
(define-key evil-normal-state-map (kbd "B") 'bury-buffer)
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "M-k") (lambda () (interactive) (suspend-tty)))
(define-key evil-normal-state-map (kbd "C-k") (lambda () (interactive) (recenter-top-bottom 0)))


(global-set-key (kbd "M-v") 'yank)

(define-key evil-normal-state-map (kbd "M-E") 'helm-resume)
;; (define-key evil-normal-state-map "'" 'helm-command-prefix)
;; (define-key evil-normal-state-map (kbd "' m") 'helm-global-mark-ring)
;; (define-key evil-normal-state-map (kbd "' k") 'helm-show-kill-ring)
;; (define-key evil-normal-state-map (kbd "' x") 'helm-M-x)
;; (define-key evil-normal-state-map (kbd "' f") 'helm-find-files)
(define-key evil-normal-state-map (kbd "M-v") 'nil)
(define-key evil-normal-state-map (kbd "C-p") 'undo-tree-redo)
(define-key evil-normal-state-map (kbd "M-v") 'lalopmak-evil-paste-below-then-normal)
(define-key evil-normal-state-map (kbd "SPC") evil-leader--default-map)

(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-w") 'other-frame)
(evil-global-set-key 'normal (kbd "M-w") 'other-frame)
(evil-global-set-key 'insert (kbd "M-w") 'other-frame)

(defun mirror-last-arg ()
    (interactive)
  (insert (concat "=" (word-at-point)))
  )
;;  keybinding already used by mail
;; (define-key evil-insert-state-map (kbd "C-M-m") 'mirror-last-arg)


 


;; Make movement keys work like they should
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

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
  )  )
   
 ;; (evil-define-key 'insert prog-mode-map (kbd "SPC") 'space-or-period)
      
(evil-define-key 'normal org-mode-map (kbd "<tab>") 'org-cycle)
(with-eval-after-load "git-commit"
  (evil-define-key 'insert with-editor-mode-map (kbd "M-k") 'git-kill-to-comments)
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


;; cursor
(setq evil-insert-state-cursor '((bar . 5) "yellow"))

(evil-global-set-key 'normal (kbd "C-M-u") 'increment-number-at-point)
(evil-global-set-key 'normal (kbd "C-M-e") 'decrement-number-at-point)

; Use magit mode's default show commit command for RET
(evil-define-key 'normal magit-blame-mode-map (kbd "RET") 'magit-show-commit)
(evil-define-key 'normal magit-blame-mode-map (kbd "SPC") 'magit-diff-show-or-scroll-up)

(evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)

(evil-define-key 'normal git-rebase-mode-map (kbd "s") (lambda () (interactivet) (git-rebase-squash)))
(evil-define-key 'normal git-rebase-mode-map (kbd "M-e") (lambda () (interactivet) (git-rebase-edit)))
(with-eval-after-load 'company
  (evil-define-key 'insert company-active-map (kbd "M-m") 'company-filter-candidates))

(global-set-key (kbd "M-,") 'pop-global-mark)
(evil-global-set-key 'normal (kbd "M-,") 'pop-global-mark)

(evil-define-key 'normal helm-ag-mode-map (kbd "RET") 'helm-ag-mode-jump)
(evil-define-key 'normal wgrep-mode-map (kbd "RET") 'helm-ag-mode-jump)

