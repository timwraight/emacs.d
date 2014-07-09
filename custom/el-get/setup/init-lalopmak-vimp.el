(require 'lalopmak-vimp)
(require 'ace-jump-mode)
(define-key vimp-normal-state-map "m" 'helm-ls-git-ls)
;; (define-key vimp-normal-state-map ";" 'helm-mini)
(define-key vimp-normal-state-map "'" 'helm-command-prefix)

(defun save-if-visiting-file ()
  (if (buffer-file-name)
    (save-buffer))
)

(add-hook 'vimp-insert-state-exit-hook 'save-if-visiting-file)