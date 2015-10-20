(with-eval-after-load 'company
  (define-key company-active-map "\t" 'company-yasnippet-or-completion)
  ;; Couldn't get these to work
  ;; (define-key company-active-map (kbd "M-e") #'company-select-next)
  ;; (define-key company-active-map (kbd "M-u") #'company-select-previous)
  )


(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))

(defun yas/expansion-at-point ()
  "Tested with v0.6.1. Extracted from `yas/expand-1'"
    (first (yas/current-key)))


(global-company-mode)
