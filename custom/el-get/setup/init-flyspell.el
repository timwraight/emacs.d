(setq flyspell-issue-message-flag nil)
(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)

(flyspell-lazy-mode 1)
