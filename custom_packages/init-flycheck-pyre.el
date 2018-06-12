

;;; Code:
(require 'flycheck)

(flycheck-define-checker python-pyre

  :command ("mypy" source)
  :predicate flycheck-buffer-saved-p
  :error-patterns
  ((error line-start (file-name) ":" line ": error:" (message) line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-pyre t)

(provide 'flycheck-pyre)

(flycheck-add-next-checker 'python-flake8 'python-pyre)
