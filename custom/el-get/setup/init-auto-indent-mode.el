(setq auto-indent-key-for-end-of-line-insert-char-then-newline "<M-return>")
(setq auto-indent-assign-indent-level 4)
(add-hook 'python-mode-hook 'auto-indent-mode)
(add-hook 'js-mode-hook 'auto-indent-mode)
