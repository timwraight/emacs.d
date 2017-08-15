(require 'company)
(with-eval-after-load 'company

  (setq company-idle-delay 0.2)
  (add-to-list 'completion-styles 'initials t)
  )

(global-company-mode)
