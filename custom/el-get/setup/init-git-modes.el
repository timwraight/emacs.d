(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'git-commit-mode-hook (lambda () (setq save-place 0)))
(setq magit-save-some-buffers 'dontask)
(setq git-commit-ignore-style-errors t)

