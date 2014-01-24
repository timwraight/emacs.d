;; HOOKS
(add-hook 'python-mode-hook
      (lambda ()
        (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'python-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(define-key python-mode-map (kbd "C-c d") 'jedi:show-doc)
(setq python-indent-offset 4)
