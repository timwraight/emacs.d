(require 'helm-dash)
(defun haskell-docs ()
  (interactive)
  (setq-local helm-dash-docsets '("Haskell")))

(add-hook 'haskell-mode-hook 'haskell-docs)
