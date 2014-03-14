(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "~/.emacs.d/snippets/yasnippets"))
(yas-global-mode 1)
(yas-reload-all)
(if (not (display-graphic-p))
    (progn
        (define-key yas-minor-mode-map (kbd "TAB") 'yas/expand)))
