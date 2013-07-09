;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-reply-to-address "Tim.Wraight@tangentlabs.co.uk"
      mu4e-user-mail-address-list '("Tim.Wraight@tangentlabs.co.uk")
      user-mail-address "Tim.Wraight@tangentlabs.co.uk"
      user-full-name  "Tim Wraight")

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
                            "RET" 'mu4e-view-message)))

(setq mu4e-maildir "~/Mail")
(add-hook 'mu4e-main-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-view-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-compose-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-headers-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-headers-mode-hook 'toggle-truncate-lines)
(setq
 mu4e-get-mail-command "offlineimap"   ;; or fetchmail, or ...
 mu4e-update-interval 60)             ;; update every 5 minutes

(setq mu4e-headers-fields
      '( (:date          .  12)
         (:flags         .   6)
         (:from          .  18)
         (:subject       .  nil)))

(setq
 mu4e-headers-visible-lines 16
 mu4e-use-fancy-chars t)

(setq mu4e-drafts-folder "/drafts")
(setq mu4e-sent-folder   "/TangentTim/INBOX.Sent Messages")
(setq mu4e-trash-folder  "/TangentTim/INBOX.Deleted Messages")

(setq mu4e-compose-complete-only-personal t)
(setq mu4e-html2text-command "html2text -utf8 -width 72")
(add-to-list 'ac-modes 'mu4e-compose-mode)

 (setq mu4e-maildir-shortcuts
       '( ("/TangentTim/INBOX" . ?i)
          ("/TangentTim/INBOX.Sent Messages" . ?s)
          ("/TangentTim/INBOX.Deleted Messages" . ?t)
          ("/TangentTim/INBOX.Archive" . ?a)
          ("/TangentTim/INBOX.Errors.Dashboard" . ?e)
          ("/TangentTim/INBOX.Wolseley Data" . ?d)))

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


;; smtp mail setting
(setq
     message-send-mail-function 'smtpmail-send-it
     smtpmail-smtp-server "mail.tangentlabs.co.uk"
     ;; if you need offline mode, set these -- and create the queue dir
     ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
     smtpmail-queue-mail  nil
     smtpmail-queue-dir  "/Users/twraight/Mail/queue/cur")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)
