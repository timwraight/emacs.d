(setq flyspell-issue-message-flag nil)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(flyspell-lazy-mode 1)

(add-hook 'text-mode-hook
          (lambda ()
            (variable-pitch-mode t)
            (local-set-key (kbd "<f2>")
                           (lambda ()
                             (interactive)
                             (flyspell-auto-correct-previous-word (point))))))
