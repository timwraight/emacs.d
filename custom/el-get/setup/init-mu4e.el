;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
;; MU4E, MU for Emacs, a mail client

;; Vimp keybindings for mu4e

(eval-after-load 'mu4e
  '(progn
     ;; use the standard bindings as a base
     (vimp-make-overriding-map mu4e-view-mode-map 'normal t)
     (vimp-make-overriding-map mu4e-main-mode-map 'normal t)
     (vimp-make-overriding-map mu4e-headers-mode-map 'normal t)
     
     (vimp-add-hjkl-bindings mu4e-view-mode-map 'normal
                             "J" 'mu4e~headers-jump-to-maildir
                             "j" 'vimp-next-line
                             "C" 'mu4e-compose-new
                             "o" 'mu4e-view-message
                             "Q" 'mu4e-raw-view-quit-buffer)
     
     ;; (vimp-add-hjkl-bindings mu4e-view-raw-mode-map 'normal
     ;; "J" 'mu4e-jump-to-maildir
     ;; "j" 'vimp-next-line
     ;; "C" 'mu4e-compose-new
     ;; "q" 'mu4e-raw-view-quit-buffer)
     
     (vimp-add-hjkl-bindings mu4e-headers-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'vimp-next-line
       "C" 'mu4e-compose-new
       "o" 'mu4e-view-message)
     
     (vimp-add-hjkl-bindings mu4e-main-mode-map 'normal
       "J" 'mu4e~headers-jump-to-maildir
       "j" 'vimp-next-line
       "RET" 'mu4e-view-message))
  )

(setq mu4e-maildir "~/Mail")
(add-hook 'mu4e-main-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-view-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-compose-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-headers-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-headers-mode-hook 'toggle-truncate-lines)
(setq
 mu4e-get-mail-command "true" ;; or fetchmail, or ...
 mu4e-headers-auto-update nil
 mu4e-update-interval 30)             ;; update every 5 minutes

(setq mu4e-headers-fields
      '( (:human-date    .  12)
         (:flags         .   6)
         (:from          .  18)
         (:subject       .  nil)))

(setq
 mu4e-headers-visible-lines 16
 mu4e-use-fancy-chars t)


(setq mu4e-compose-complete-only-personal t)
(setq mu4e-html2text-command "html2text -utf8 -width 72")
(add-to-list 'ac-modes 'mu4e-compose-mode)

(setq mu4e-attachment-dir  "~/Downloads")



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
