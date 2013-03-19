;; SMEX  (IDO for meta-x)
(require 'smex)
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))


;; PROLOG

(require 'ediprolog)
(add-hook 'prolog-mode-hook
          (lambda ()
             (print "Hello")
             (local-set-key (kbd "C-c C-c") 'ediprolog-dwim)))



;; HTML
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'html-mode-hook (lambda ()
                            (variable-pitch-mode 0)
                            (turn-off-flyspell)
                            (setq truncate-lines t)))


;; ReST mode
(add-hook 'rst-mode-hook (lambda ()
                           (variable-pitch-mode 0)))
(add-hook 'rst-adjust-hook 'rst-toc-update)


;; SASS-MODE
(require 'sass-mode)


;; HELP

(global-set-key (kbd "C-h a") 'apropos)

;; GIT

(global-set-key (kbd "C-x g") 'magit-status)
(require 'magit)
(add-hook 'magit-log-edit-mode-hook 'vimp-insert-state)


;; SEARCHING 

(define-key global-map "\C-x\C-r" 'rgrep)

;; MODES

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js-mode))


;; XML

(add-hook 'nxml-mode-hook (lambda () (variable-pitch-mode nil)))

;; JAVASCRIPT
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(require 'flymake-jshint)
(add-hook 'js-mode-hook
          (lambda ()
            (interactive)
            (flymake-mode)
            (setq truncate-lines 0)))

;; CSS

(require 'flymake-csslint)
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))



;; BROWSING

(require 'w3m-load)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)


;; Markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; PHP

(require 'php-mode)
(defun flymake-php-init ()
  "Use php to check the syntax of the current file."
  (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	 (local (file-relative-name temp (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local "-l"))))

(add-to-list 'flymake-err-line-patterns 
             '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))

(add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))
(add-hook 'php-mode-hook (lambda () (flymake-mode 1)))


;; TEX
(load "auctex.el" nil t t)


;; YAML
(require 'yaml-mode)
(setq yaml-indent-offset 2)


;; SLIME
(setq inferior-lisp-program "/usr/local/bin/clisp") ; your Lisp system
(require 'slime)
(slime-setup)

;; SCHEME
(load-file "~/.emacs.d/vendor/geiser/elisp/geiser.el")
(setq geiser-active-implementations '(guile))
(add-hook 'scheme-mode-hook
          (lambda ()
            (geiser-mode)
            (run-guile)))

;; CLOJURE

(require 'clojure-mode)
(require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook
  'nrepl-turn-on-eldoc-mode)

;; HASKELL
(load "~/.emacs.d/vendor/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; SQL

(add-hook 'sql-mode-hook
          (lambda ()
            (setq truncate-lines t)
            (sql-highlight-mysql-keywords)))


;; ESHELL
(setq eshell-prompt-function
      (lambda nil
        (concat (car
                 (last
                  (split-string (eshell/pwd) "/")))
                " ")))

(setq eshell-prompt-regexp "^[^#$\n]* ")


;; MARKDOWN

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.text" . markdown-mode) auto-mode-alist))


;; FORTH
(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(add-to-list 'auto-mode-alist '("\\.fs$" . forth-mode))


; ERC

(add-hook 'erc-join-hook (lambda ()
                           (interactive)
                           (variable-pitch-mode)))


; TRAMP

(setq tramp-default-method "ssh")
(setq tramp-syntax 'url)
(load-user-file "tramp.el")

; ACK

(require 'ack)
(autoload 'pcomplete/ack "pcmpl-ack")
(autoload 'pcomplete/ack-grep "pcmpl-ack")


;; SSH CONFIG MODE
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '(".ssh/config\\'"  . ssh-config-mode))
(add-to-list 'auto-mode-alist '("sshd?_config\\'" . ssh-config-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

