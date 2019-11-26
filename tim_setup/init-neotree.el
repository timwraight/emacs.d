(require 'neotree)
(global-set-key [f6] 'neotree-toggle)
(add-hook 'neotree-mode-hook 'switch-buffer-to-monospaced)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)