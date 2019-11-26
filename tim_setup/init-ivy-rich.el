(require 'ivy-rich)
(ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
(setq ivy-virtual-abbreviate 'full
      ivy-rich-switch-buffer-align-virtual-buffer t)
(setq ivy-rich-abbreviate-paths t)