(require 'eclim)
(global-eclim-mode -1)
(require 'eclimd)
;; regular auto-complete initialization
(require 'auto-complete-config)
(ac-config-default)
(add-hook 'java-mode-hook 'eclim-mode)
(add-hook 'java-mode-hook (
     lambda ()
         (local-set-key (kbd "C-c .") 'eclim-java-find-declaration)))
(vimp-leader/set-key-for-mode 'java-mode "." 'eclim-java-find-declaration)

;; show errors when on top of them
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
(add-hook 'eclim-mode-hook 'eclim-problems-highlight)

;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
