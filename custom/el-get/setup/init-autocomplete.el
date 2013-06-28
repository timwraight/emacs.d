;; AUTOCOMPLETE

; Load the default configuration
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")
(ac-config-default)

(ac-flyspell-workaround)
(setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")

;; (global-auto-complete-mode t)
(setq ac-auto-show-menu t)
(setq ac-dwim t)
(setq ac-use-menu-map t)
(setq ac-quick-help-delay 1)
(setq ac-quick-help-height 60)
(setq ac-auto-start 2)
(setq ac-candidate-menu-min 2)
(define-key ac-menu-map (kbd "C-<SPC>") 'ac-isearch)

;; don't ignore case
(setq ac-ignore-case nil)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-abbrev
               ac-source-yasnippet))

(dolist (mode '(log-edit-mode haml-mode sass-mode yaml-mode csv-mode espresso-mode
                haskell-mode html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

(add-hook 'text-mode-hook (lambda () (setq auto-complete-mode nil)))
(add-hook 'comint-mode-hook 'auto-complete-mode)

