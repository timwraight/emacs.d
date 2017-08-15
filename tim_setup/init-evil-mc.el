(require 'evil-mc)
(evil-define-key 'normal evil-mc-key-map (kbd "M-n") 'evil-backward-little-word-begin)
(evil-define-key 'normal evil-mc-key-map (kbd "C-p") 'undo-tree-redo)

(setq evil-mc-undo-cursors-on-keyboard-quit t)
(global-evil-mc-mode 1)
(add-hook 'evil-mc-mode-hook 'evil-mc-pause-cursors)
