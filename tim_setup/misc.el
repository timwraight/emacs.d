

;; Typing break mode, to remind me to take breaks
;; (type-break-mode)


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
;; We don't always want JSHint. We're trying out eslint at the mo
;; (add-hook 'js-mode-hook (lambda() (flycheck-select-checker 'javascript-jshint)))



;; HASKELL
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; SQL

(add-hook 'sql-mode-hook
          (lambda ()
            (sql-highlight-postgres-keywords)
            ))

(eval-after-load "sql-mode"
  '(progn
     (define-key sql-interactive-mode-map (kbd "M-u") 'comint-previous-input)
     (define-key sql-interactive-mode-map (kbd "M-e") 'comint-next-input)))

(defun shell-command-on-buffer (command)
  "Asks for a command and executes it in inferior shell with current buffer
as input."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   command t t))

(with-eval-after-load 'sql-mode
  (define-key sql-mode-map (kbd "M-q") (lambda () (interactive) (shell-command-on-buffer "pg_format")))
)
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

;; JUMBLE OF STUFF PREVIOUSLY IN SETUP-SPECIFIC
(setq 
 alert-default-style (quote notifier)
 ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"]
 auto-indent-key-for-end-of-line-insert-char-then-newline "M-RET"
 bookmark-save-flag 1
 buffer-face-mode-face (quote fixed-pitch)
 confluence-save-credentials t
 custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" "2025fdb1712b921f60f1a7d673fefb5c397a37ba5a6e2d21b1589c4372bb9733" "b19b642b0d5be8ec4bc96698260575d3eb81a22064911a8036213facf3a9a6fa" default))
 elscreen-display-screen-number nil
 elscreen-tab-display-control nil
 elscreen-tab-display-kill-screen nil
 fci-rule-color "#383838"
 flycheck-disabled-checkers (quote (css-csslint))
 haskell-process-auto-import-loaded-modules t
 haskell-process-log t
 haskell-process-suggest-remove-import-lines t
 helm-ff-transformer-show-only-basename nil
 holiday-bahai-holidays nil
 holiday-islamic-holidays nil
 jabber-alert-presence-hooks nil
 jabber-auto-reconnect t
 jabber-backlog-days 7.0
 magit-diff-refine-hunk nil
 magit-diff-section-arguments (quote ("--no-ext-diff"))
 magit-push-always-verify nil
 mu4e-split-view nil
 multi-term-program "/usr/local/bin/zsh"
 ns-right-command-modifier (quote meta)
 org-agenda-include-diary t
 org-agenda-span (quote day)
 org-agenda-start-with-clockreport-mode nil
 org-agenda-start-with-log-mode nil
 org-list-allow-alphabetical t
 org-time-clocksum-format
   (quote
    (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
 password-cache-expiry 28800
 python-indent-offset 4
 shr-color-visible-distance-min 10
 shr-color-visible-luminance-min 80
 sql-product (quote postgres)
 term-bind-key-alist
   (quote
    (("M-y" . term-send-right)
     ("M-l" . term-send-left)
     ("C-c C-c" . term-interrupt-subjob)
     ("C-c C-e" . term-send-esc)
     ("C-p" . previous-line)
     ("C-n" . next-line)
     ("C-s" . isearch-forward)
     ("C-r" . isearch-backward)
     ("C-m" . term-send-return)
     ("C-y" . term-paste)
     ("M-i" . term-send-forward-word)
     ("M-n" . term-send-backward-word)
     ("M-u" . term-send-up)
     ("M-e" . term-send-down)
     ("M-d" . term-send-forward-kill-word)
     ("M-<backspace>" . term-send-backward-kill-word)
     ("<C-backspace>" . term-send-backward-kill-word)
     ("M-r" . term-send-reverse-search-history)
     ("M-." . comint-dynamic-complete)))
 undo-tree-history-directory-alist (quote (("." . "/tmp")))
 vc-annotate-background "#2B2B2B"
 vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 vc-annotate-very-old-color "#DC8CC3"
 vc-follow-symlinks t
 vimp-magic (quote very-magic)
 yaml-indent-offset 2)




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
