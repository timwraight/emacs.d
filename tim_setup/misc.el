

;; Typing break mode, to remind me to take breaks
(type-break-mode)


 ;; Semantic mode
(semantic-mode)

;; Abbrev mode (to help me write faster)
(setq-default abbrev-mode nil)
(setq abbrev-mode nil)

;; HTML
(add-hook 'sgml-mode-hook (lambda ()
                            (interactive)
                            (buffer-switch-to-monospaced)
                            (toggle-truncate-lines)
                            (emmet-mode)
                            (turn-off-flyspell)))
(setq sgml-basic-offset 4)

;; EDIFF

(defvar my-ediff-last-windows nil)

(defun my-store-pre-ediff-winconfig ()
  (setq my-ediff-last-windows (current-window-configuration)))

(defun my-restore-pre-ediff-winconfig ()
  (set-window-configuration my-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'my-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'my-restore-pre-ediff-winconfig)

;; ReST mode
(add-hook 'rst-mode-hook (lambda ()
                           (variable-pitch-mode 0)))
(add-hook 'rst-adjust-hook 'rst-toc-update)


;; HELP

(global-set-key (kbd "C-h a") 'apropos)


;; XML
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl\\'" . nxml-mode))
(add-hook 'nxml-mode-hook 'buffer-switch-to-monospaced)

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
            (sql-highlight-postgres-keywords)
            (auto-complete-mode)))

(eval-after-load "sql-mode"
  '(progn
     (define-key sql-interactive-mode-map (kbd "M-u") 'comint-previous-input)
     (define-key sql-interactive-mode-map (kbd "M-e") 'comint-next-input)))

;; Wee snippet for incrementing month values using
;; query-replace-regexp-eval

;; (add-hook 'sql-interactive-mode-hook
;;           (lambda ()
;;             (interactive)
;;             (setq sql-prompt-regexp "\\[_[:alpha:]]=>")
;;             (setq sql-prompt-cont-regexp "\\[_[:alpha:]]->")))

(add-hook 'sql-interactive-mode-hook (lambda ()
                           (interactive)
                           (toggle-truncate-lines)
                           (setq comint-prompt-regexp "[[:alpha:]_-]+(-|=)> ")))


;; Shell script editing
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; ESHELL
(setq eshell-prompt-function
      (lambda nil
        (concat (car
                 (last
                  (split-string (eshell/pwd) "/")))
                ": ")))

(setq eshell-prompt-regexp "^[^#$\n]* :")
(setenv "DYLD_LIBRARY_PATH" "/usr/local/mysql/lib:$DYLD_LIBRARY_PATH")


; ERC

(add-hook 'erc-join-hook (lambda ()
                           (interactive)
                           (variable-pitch-mode)))


; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")
(setq tramp-syntax 'ftp)


; INFO
(setq Info-default-directory-list
             (append Info-default-directory-list
                     '("~/info")))


; Doc function for elisp
;;; describe this point lisp only
(defun describe-foo-at-point ()
        "Show the documentation of the Elisp function and variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
    (interactive)
    (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
                            (with-syntax-table emacs-lisp-mode-syntax-table
                                (save-excursion
                                (or (not (zerop (skip-syntax-backward "_w")))
                                    (eq (char-syntax (char-after (point))) ?w)
                                    (eq (char-syntax (char-after (point))) ?_)
                                    (forward-sexp -1))
                                (skip-chars-forward "`'")
                            (let ((obj (read (current-buffer))))
                                    (and (symbolp obj) (fboundp obj) obj))))))
                (describe-function sym))
                ((setq sym (variable-at-point)) (describe-variable sym))
                ;; now let it operate fully -- i.e. also check the
                ;; surrounding sexp for a function call.
                ((setq sym (function-at-point)) (describe-function sym)))))
