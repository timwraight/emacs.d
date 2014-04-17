(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippets")

(yas-global-mode 1)
(yas-reload-all)
(if (not (display-graphic-p))
    (progn
        (define-key yas-minor-mode-map (kbd "TAB") 'yas/expand)))
