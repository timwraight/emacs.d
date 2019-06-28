; Make sure to actually define the switch to monospaced function, or
;; the next bit won't work
;; do it in setup-specific so that you can use a font you actually have.
;; You can use code like this:
;;

(add-hook 'python-mode-hook 'buffer-switch-to-monospaced)
(add-hook 'inferior-python-mode-hook 'buffer-switch-to-monospaced)
(add-hook 'python-mode-hook 'electric-indent-local-mode)

(defun tw/isort-and-blacken ()
  (py-isort-buffer)
  (blacken-buffer)
  )

(add-hook 'python-mode-hook
          (lambda () (add-hook 'before-save-hook 'tw/isort-and-blacken nil 'local)))

(setq python-indent-offset 4)
;; (with-eval-after-load "vimp"
;;   (evil-define-key 'insert python-mode-map (kbd "M-h") 'rope-lucky-assist))

;; ;; Autofill inside of comments
;; (setq-mode-local python-mode
;;                  comment-auto-fill-only-comments t
;;                  comment-fill-column 99
;;                  )
(add-to-list 'auto-mode-alist '("\\.pyi\\'" . python-mode))


(add-hook 'python-mode-hook (lambda () (interactive) (setq comment-fill-column 99)))

;; (add-hook 'python-mode-hook (lambda () (interactive)
;;                               (if (buffer-file-name)
;;                                   (progn
;;                                     (hs-minor-mode)
;;                                     (hs-hide-all)))
;;                               ))

(defun send-to-pony-shell ()
  (interactive)
  (comint-send-string (get-buffer-process "*ponysh*") (concat (buffer-substring (region-beginning) (region-end)) "\n"))
  )

(add-hook 'python-mode-hook (ggtags-mode 1))

(add-hook 'semantic-init-hook
          (lambda ()
            (interactive)
            (if (and (bound-and-true-p python-mode) (buffer-file-name))
                (helm-semantic nil))))
