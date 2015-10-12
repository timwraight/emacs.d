(eval-after-load "direx-mode"
  '(progn 
     (define-key direx:direx-mode-map (kbd "j") 'direx:next-item)
     (define-key direx:direx-mode-map (kbd "k") 'direx:previous-item)
     (define-key direx:direx-mode-map (kbd "h") 'direx:up-item)
     (define-key direx:direx-mode-map (kbd "l") 'direx:down-item)
     (define-key direx:direx-mode-map (kbd "o") 'direx:maybe-find-item)
     (define-key direx:direx-mode-map (kbd "m") 'helm-ls-git-ls)
     (define-key direx:direx-mode-map (kbd "<SPC>") 'previous-buffer)))
