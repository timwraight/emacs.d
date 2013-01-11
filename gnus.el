; Also check .gnus. 
(require 'gnus-load)
(add-hook 'gnus-article-mode-hook (lambda () (variable-pitch-mode t)))

; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "")

(setq gnus-select-method 
      '(nnmaildir "GMail" 
                  (directory "~/Documents/mail/")
                  (directory-files nnheader-directory-files-safe) 
                  (get-new-mail nil)))


(load "tls")
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      starttls-gnutls-program "/usr/local/bin/gnutls-cli"
      starttls-extra-arguments nil      
      smtpmail-starttls-credentials 
      '(("smtp.gmail.com" 587 "timothywraight@.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t
      smtpmail-local-domain "wraight.net")


(setq user-mail-address "tim@wraight.net")
(setq user-full-name "Tim Wraight")
(setq mm-discouraged-alternatives '("text/html" "text/richtext"))
(setq gnus-always-read-dribble-file t)

