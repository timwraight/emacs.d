;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
;; MU4E, MU for Emacs, a mail client

;; Vim keybindings for mu4e
(eval-after-load 'mu4e
  '(progn
     (add-to-list 'mu4e-view-actions
		  '("ViewInBrowser" . mu4e-action-view-in-browser) t)


     ;; Message box
     (define-key mu4e-headers-mode-map (kbd "e") 'mu4e-headers-next)
     (define-key mu4e-headers-mode-map (kbd "u") 'mu4e-headers-prev)
     (define-key mu4e-headers-mode-map (kbd "i") 'mu4e-headers-view-message)
     (define-key mu4e-headers-mode-map (kbd "l") 'mu4e-headers-mark-for-unmark)
     (define-key mu4e-headers-mode-map (kbd "n") 'mu4e-headers-mark-for-refile)

     (define-key mu4e-headers-mode-map (kbd "c") 'mu4e-compose-new)

     (define-key mu4e-headers-mode-map (kbd "M-m") 'mu4e-headers-mark-for-something)
     (define-key mu4e-headers-mode-map (kbd "r") 'mu4e-headers-rerun-search)
     (define-key mu4e-headers-mode-map (kbd "m") 'helm-timi)
     (define-key mu4e-headers-mode-map (kbd "SPC") evil-leader--default-map)
     (define-key mu4e-view-mode-map (kbd "SPC") evil-leader--default-map)


     ;; Search result navigation
     (define-key mu4e-headers-mode-map (kbd "M-n") 'mu4e-headers-query-prev)
     (define-key mu4e-headers-mode-map (kbd "M-i") 'mu4e-headers-query-next)
     ;; Individual Message
     (define-key mu4e-view-mode-map (kbd "e") 'mu4e-view-headers-next)
     (define-key mu4e-view-mode-map (kbd "u") 'mu4e-view-headers-prev)
     (define-key mu4e-view-mode-map (kbd "l") 'mu4e~view-quit-buffer)
     (define-key mu4e-view-mode-map (kbd "n") 'mu4e-view-mark-for-refile)
     (define-key mu4e-view-mode-map (kbd "M-s") 'mu4e-view-save-attachment)
     (define-key mu4e-view-mode-map (kbd "m") 'helm-timi)
     (define-key mu4e-view-mode-map (kbd "A") 'evil-visual-line)


     (require 'org-mu4e)
     ))
(setq mu4e-split-view 'vertical)
(setq mu4e-headers-leave-behavior 'apply)
(setq mu4e-use-fancy-chars t)
(setq mu4e-view-show-images t)


(setq mu4e-maildir "~/Mail")
(setq mu4e-headers-skip-duplicates t)
(add-hook 'mu4e-main-mode-hook 'evil-local-mode)
(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
(add-hook 'mu4e-compose-mode-hook 'evil-insert-state)
(add-hook 'mu4e-headers-mode-hook 'evil-local-mode)
(add-hook 'mu4e-headers-mode-hook 'toggle-truncate-lines)
(add-hook 'mu4e-headers-mode-hook (lambda () (interactive) (variable-pitch-mode t)))

(setq mu4e-headers-fields
    '( (:human-date    .  15)    ;; alternatively, use :human-date
       (:flags         .   6)
       (:from          .  22)
       (:subject       .  50)))
(setq mu4e-headers-visible-columns 140)

;; Flow text nicely for outgoing emails
(add-hook 'mu4e-compose-mode-hook
          (defun cpb-compose-setup ()
            "Outgoing mails get format=flowed."
            (use-hard-newlines t 'guess)))

(require 'mu4e-contrib)
(setq mu4e-html2text-command 'mu4e-shr2text)


;; Convert plain text in email to html via markdown
(defun mimedown ()
  (interactive)
  (save-excursion
    (message-goto-body)
    (shell-command-on-region (point) (point-max) "mimedown.py" nil t)))



                                        ; mu4e org stuff
(setq message-citation-line-format "* On %Y-%m-%d at %R, %f wrote:")
