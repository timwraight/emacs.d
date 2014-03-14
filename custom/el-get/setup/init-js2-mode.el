(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(eval-after-load "js2-mode"
  '(progn
     (setq js2-mirror-mode nil)
     (key-chord-define js2-mode-map ",/" 'helm-javascript-functions)
     (define-key js2-mode-map (kbd "<M-RET>") 'auto-indent-eol-char-newline)
     ;; Let flycheck handle parse errors
     (setq-default js2-show-parse-errors nil)
     (setq-default js2-strict-missing-semi-warning nil)
     (setq-default js2-mode-show-strict-warnings nil)
     (setq-default js2-strict-cond-assign-warning nil)
     (setq-default js2-highlight-undeclared-vars nil)
     (setq-default js2-strict-trailing-comma-warning nil)))
