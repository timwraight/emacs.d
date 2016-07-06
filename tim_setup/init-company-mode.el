(require 'company)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-i") 'company-complete-selection)
  (define-key company-active-map (kbd "M-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "M-e") 'company-select-next)
  (define-key company-active-map (kbd "M-t") 'company-abort)
  (define-key company-active-map (kbd "M-u") 'company-select-previous)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-l") 'company-show-location)
  (setq company-idle-delay 0.2)
  )

(global-company-mode)
