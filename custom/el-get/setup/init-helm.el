(setq helm-ls-git-show-abs-or-relative 'relative)
(setq helm-ff-transformer-show-only-basename nil)

(setq helm-display-function
      (lambda (buf)
        (switch-to-buffer buf)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))

(setq helm-truncate-lines t)


(defun helm-javascript-functions ()
    "Display headlines for the current javascript file."                                    (interactive)                                                                           (helm-mode t)
    (helm :sources '(((name . "Javascript Functions")
                      (volatile)
                      (headline "\\([a-z]+: function\\|function [a-z]+\\)")))))


(require 'helm)
(define-key helm-map (kbd "C-j") 'helm-next-line)
(define-key helm-map (kbd "C-k") 'helm-previous-line)

