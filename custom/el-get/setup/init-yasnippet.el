(yas-global-mode 1)
(if (not (display-graphic-p))
    (progn
        (define-key yas-minor-mode-map (kbd "TAB") 'yas/expand)))
