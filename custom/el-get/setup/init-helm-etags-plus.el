(autoload 'turn-on-ctags-auto-update-mode "ctags-update" "turn on 'ctags-auto-update-mode'." t)
(add-hook 'js2-mode-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'python-mode-hook 'turn-on-ctags-auto-update-mode)
(autoload 'ctags-update "ctags-update" "update TAGS using ctags" t)
(turn-on-ctags-auto-update-mode)
