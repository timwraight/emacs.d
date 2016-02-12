(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")
; Make sure to actually define the switch to monospaced function, or
;; the next bit won't work
;; do it in setup-specific so that you can use a font you actually have.
;; You can use code like this:
;;
;; (defun buffer-switch-to-monospaced ()
;;   (interactive)
;;   (face-remap-add-relative 'default :family "Consolas")
;;   )

(add-hook 'python-mode-hook 'buffer-switch-to-monospaced)
(add-hook 'python-mode-hook 'electric-indent-local-mode)


