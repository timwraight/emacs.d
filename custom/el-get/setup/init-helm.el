(setq helm-ls-git-show-abs-or-relative 'relative)
(setq helm-ff-transformer-show-only-basename nil)

(setq helm-display-function
      (lambda (buf)
        (switch-to-buffer buf)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))

(add-hook 'helm-after-initialize-hook
                 (lambda ()
                        (toggle-truncate-lines t)))
