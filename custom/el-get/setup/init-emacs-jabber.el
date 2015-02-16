(require 'jabber-autoloads)
(setq jabber-default-connection-type 'network)
(setq jabber-mode-line-presence)
(setq jabber-auto-reconnect t)
(setq jabber-history-enabled t)
(setq jabber-alert-presence-message-function nil)

(require 'alert)

(defun notify-jabber-notify (from buf text proposed-alert)
  "(jabber.el hook) Notify of new Jabber chat messages via notify.el"
  (when (or jabber-message-alert-same-buffer
            (not (memq (selected-window) (get-buffer-window-list buf))))
    (if (jabber-muc-sender-p from)
        (alert (format "(PM) %s"
                      (jabber-jid-displayname (jabber-jid-user from)))
               (format "%s: %s" (jabber-jid-resource from) text)))
      (alert (format "%s" (jabber-jid-displayname from))
             text)))

(add-hook 'jabber-alert-message-hooks 'notify-jabber-notify)
(add-hook 'jabber-chat-mode-hook 'vimp-insert-state)
(add-hook 'jabber-chat-mode-hook 'longlines-mode)
