;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (interactive)
                                (variable-pitch-mode nil)
                                (auto-fill-mode)))
(eval-after-load "markdown-mode"
  '(define-key markdown-mode-map (kbd "M-RET") 'markdown-insert-list-item))
