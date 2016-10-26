(setq python-shell-interpreter "python")
; Make sure to actually define the switch to monospaced function, or
;; the next bit won't work
;; do it in setup-specific so that you can use a font you actually have.
;; You can use code like this:
;;

(add-hook 'python-mode-hook 'buffer-switch-to-monospaced)
(add-hook 'python-mode-hook 'electric-indent-local-mode)
(setq python-indent-offset 4)
;; (with-eval-after-load "vimp"
;;   (vimp-define-key 'insert python-mode-map (kbd "M-h") 'rope-lucky-assist))

;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode nil)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-syntax-comment-or-string-p)))))

(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)
            ))
