(elscreen-start)
(elscreen-persist-mode 1)
(elscreen-persist-restore)
(add-hook 'kill-emacs-hook 'elscreen-persist-store)
