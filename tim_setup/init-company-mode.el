(require 'company)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-i") 'company-complete-selection)
  (define-key company-active-map (kbd "M-s") 'company-filter-candidates)
  (define-key company-active-map (kbd "M-e") 'company-select-next)
  (define-key company-active-map (kbd "M-n") 'company-abort)
  (define-key company-active-map (kbd "M-u") 'company-select-previous)
  (define-key company-active-map (kbd "M-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-l") 'company-show-location)
  (define-key company-active-map (kbd "M-m") 'company-filter-candidates)

  (define-key company-active-map [?\M-i] 'company-complete-selection)
  (define-key company-active-map [?\M-s] 'company-filter-candidates)
  (define-key company-active-map [?\M-e] 'company-select-next)
  (define-key company-active-map [?\M-t] 'company-abort)
  (define-key company-active-map [?\M-u] 'company-select-previous)
  (define-key company-active-map [?\M-d] 'company-show-doc-buffer)
  (define-key company-active-map [?\M-l] 'company-show-location)

  (define-key company-filter-map (kbd "M-e") 'company-select-next)
  (define-key company-filter-map (kbd "M-u") 'company-select-previous)
  (define-key company-search-map (kbd "<SPC>") 'company-abort)

  (define-key company-search-map (kbd "M-e") 'company-select-next)
  (define-key company-search-map (kbd "M-u") 'company-select-previous)
  (define-key company-search-map (kbd "<SPC>") 'company-abort)


  (setq company-idle-delay 0.2)
  (add-to-list 'completion-styles 'initials t)
  )

(global-company-mode)

