(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
(add-hook 'python-mode-hook  'turn-on-ctags-auto-update-mode)
