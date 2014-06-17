;; HTML
(add-hook 'sgml-mode-hook (lambda ()
                            (interactive)
                            (variable-pitch-mode 0)
                            (toggle-truncate-lines)
                            (emmet-mode)
                            (turn-off-flyspell)
                            (tagedit-mode)))
(setq sgml-basic-offset 4)

;; ReST mode
(add-hook 'rst-mode-hook (lambda ()
                           (variable-pitch-mode 0)))
(add-hook 'rst-adjust-hook 'rst-toc-update)


;; HELP

(global-set-key (kbd "C-h a") 'apropos)


;; XML

(add-hook 'nxml-mode-hook (lambda () (variable-pitch-mode nil)))

;; CSS

(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))

;; PHP
(setq php-mode-force-pear t)

;; VAGRANT FILES
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))

;; JAVASCRIPT
(add-hook 'js-mode-hook (lambda() (flycheck-select-checker 'javascript-jshint)))



;; HASKELL
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; SQL

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-mysql-keywords)
            (auto-complete-mode)))

(define-key sql-interactive-mode-map (kbd "M-u") 'comint-previous-input)
(define-key sql-interactive-mode-map (kbd "M-e") 'comint-next-input)

;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (interactive)
;;             (setq sql-prompt-regexp "\\[_[:alpha:]]=>")
;;             (setq sql-prompt-cont-regexp "\\[_[:alpha:]]->")))

(add-hook 'sql-interactive-mode-hook (lambda ()
                           (interactive)
                           (toggle-truncate-lines)
                           (setq comint-prompt-regexp "[[:alpha:]_-]+(-|=)> ")))

;; ESHELL
(setq eshell-prompt-function
      (lambda nil
        (concat (car
                 (last
                  (split-string (eshell/pwd) "/")))
                ": ")))

(setq eshell-prompt-regexp "^[^#$\n]* :")


; ERC

(add-hook 'erc-join-hook (lambda ()
                           (interactive)
                           (variable-pitch-mode)))


; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'ftp)
