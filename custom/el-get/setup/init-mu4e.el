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
     (define-key mu4e-headers-mode-map (kbd "m") 'mu4e-headers-rerun-search)
     ;; Search result navigation
     (define-key mu4e-headers-mode-map (kbd "M-n") 'mu4e-headers-query-prev)
     (define-key mu4e-headers-mode-map (kbd "M-i") 'mu4e-headers-query-next)
     ;; Individual Message
     (define-key mu4e-view-mode-map (kbd "e") 'mu4e-view-headers-next)
     (define-key mu4e-view-mode-map (kbd "u") 'mu4e-view-headers-prev)
     (define-key mu4e-view-mode-map (kbd "l") 'mu4e~view-quit-buffer)
     (define-key mu4e-view-mode-map (kbd "n") 'mu4e-view-mark-for-refile)
     (define-key mu4e-view-mode-map (kbd "M-s") 'mu4e-view-save-attachment)
     (define-key mu4e-view-mode-map (kbd "A") 'vimp-visual-line)))

(setq mu4e-split-view nil)
(setq mu4e-headers-leave-behavior 'apply)
(setq mu4e-html2text-command
        "html2text.py | grep -v '&nbsp_place_holder;'")
(setq mu4e-use-fancy-chars t)

(setq mu4e-maildir "~/Mail")
(setq mu4e-headers-skip-duplicates t)
(add-hook 'mu4e-main-mode-hook 'vimp-local-mode)
(add-hook 'mu4e-view-mode-hook 'vimp-local-mode)
(add-hook 'mu4e-compose-mode-hook 'vimp-insert-state)
(add-hook 'mu4e-headers-mode-hook 'vimp-local-mode)
(add-hook 'mu4e-headers-mode-hook 'toggle-truncate-lines)
