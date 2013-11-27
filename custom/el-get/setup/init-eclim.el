(require 'eclim)
(global-eclim-mode -1)
(require 'eclimd)
;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)
(add-hook 'java-mode-hook 'eclim-mode)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
