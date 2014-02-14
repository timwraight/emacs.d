;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
;; MU4E, MU for Emacs, a mail client

;; Vim keybindings for mu4e
(eval-after-load 'mu4e
  '(progn
     ;; Message box
     (define-key mu4e-headers-mode-map (kbd "j") 'mu4e-headers-next)
     (define-key mu4e-headers-mode-map (kbd "k") 'mu4e-headers-prev)
     (define-key mu4e-headers-mode-map (kbd "o") 'mu4e-headers-view-message)
     ;; Individual Message
     (define-key mu4e-view-mode-map (kbd "j") 'next-line)
     (define-key mu4e-view-mode-map (kbd "k") 'previous-line)
     (define-key mu4e-view-mode-map (kbd "V") 'vimp-visual-line)))


(setq mu4e-maildir "~/Mail")
(setq mu4e-headers-skip-duplicates t)
(add-hook 'mu4e-main-mode-hook 'vimp-local-mode)
(add-hook 'mu4e-view-mode-hook 'vimp-local-mode)
(add-hook 'mu4e-compose-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-headers-mode-hook 'vimp-local-mode)
(add-hook 'mu4e-headers-mode-hook 'toggle-truncate-lines)

(setq mu4e-headers-fields
      '((:human-date    .  12)
        (:flags         .   6)
        (:from          .  18)
        (:subject       .  nil)))

(setq
 mu4e-headers-visible-lines 16
 mu4e-use-fancy-chars t)


(setq mu4e-compose-complete-only-personal t)
(setq mu4e-html2text-command "html2text")
;; (setq mu4e-html2text-command "html2text -utf8 -width 72")

(setq mu4e-attachment-dir  "~/Downloads")

(require 'mu4e)
(add-to-list 'mu4e-view-actions
             '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Attaching messages

(require 'gnus-dired)
;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(setq mu4e-headers-leave-behavior 'apply)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
