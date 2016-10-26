; FLYCHECK
(global-flycheck-mode)
 (setq flycheck-disabled-checkers '(css-csslint))
(flycheck-add-next-checker 'python-flake8 'python-pylint)
