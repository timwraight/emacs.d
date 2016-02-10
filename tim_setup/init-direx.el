(eval-after-load "direx-mode"
  '(progn 
     (define-key direx:direx-mode-map (kbd "e") 'direx:next-item)
     (define-key direx:direx-mode-map (kbd "u") 'direx:previous-item)
     (define-key direx:direx-mode-map (kbd "n") 'direx:up-item)
     (define-key direx:direx-mode-map (kbd "i") 'direx:down-item)
     (define-key direx:direx-mode-map (kbd "RET") 'direx:maybe-find-item)
     (define-key direx:direx-mode-map (kbd "m") 'helm-ls-git-ls)
     (define-key direx:direx-mode-map (kbd "<SPC>") 'vimp-leader--default-map)
     (define-key direx:direx-mode-map (kbd "o") 'lalopmak-vimp-ace-jump-line-mode)
     ))
