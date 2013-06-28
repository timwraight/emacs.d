;; VIMP
(vimp-mode 1)
(setq vimp-auto-indent nil)
(global-set-key (kbd "<f1>") 'vimp-local-mode)

; Use visual lines with j and k
(define-key vimp-motion-state-map "j" 'vimp-next-visual-line)
(define-key vimp-motion-state-map "k" 'vimp-previous-visual-line)

(define-key vimp-insert-state-map "k" #'tim/maybe-exit)

(vimp-define-command tim/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))


; Make RET and SPACE do default Emacsy things instead of vim-movement

(defun my-move-key (keymap-from keymap-to key)
    "Moves key binding from one keymap to another, deleting from the old location. "
    (define-key keymap-to key (lookup-key keymap-from key))
    (define-key keymap-from key nil)
    )
  (my-move-key vimp-motion-state-map vimp-normal-state-map (kbd "RET"))
  (my-move-key vimp-motion-state-map vimp-normal-state-map " ")
