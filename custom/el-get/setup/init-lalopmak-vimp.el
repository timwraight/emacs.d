(require 'lalopmak-vimp)
(require 'ace-jump-mode)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)

(defun save-if-visiting-file ()
  (if (buffer-file-name)
    (save-buffer))
)

;; Is this causing freezes?
;; (add-hook 'vimp-insert-state-exit-hook 'save-if-visiting-file)
