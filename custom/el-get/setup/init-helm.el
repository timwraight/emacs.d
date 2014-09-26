(require 'helm-config)
(setq helm-ls-git-show-abs-or-relative 'relative)
(setq helm-ff-transformer-show-only-basename nil)

(setq helm-c-locate-command "mdfind -name %s %s")

(setq read-buffer-function 'ido-read-buffer)
(setq read-file-name-function 'ido-read-file-name)

(setq helm-display-function
      (lambda (buf)
        (switch-to-buffer buf)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))

(setq helm-truncate-lines t)

(require 'helm)
(require 'helm-org)
(require 'helm-files)
(define-key helm-map (kbd "M-e") 'helm-next-line)
(define-key helm-map (kbd "M-u") 'helm-previous-line)
(define-key helm-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-generic-files-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-generic-files-map (kbd "M-/") 'helm-ff-run-grep)
