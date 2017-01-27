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

;; ;; Autofill inside of comments
;; (setq-mode-local python-mode
;;                  comment-auto-fill-only-comments t
;;                  comment-fill-column 99
;;                  )

(add-hook 'python-mode-hook (lambda () (interactive) (setq comment-fill-column 99)))

(defun send-to-pony-shell ()
  (interactive)
  (comint-send-string (get-buffer-process "*ponysh*") (concat (buffer-substring (region-beginning) (region-end)) "\n"))
  )
