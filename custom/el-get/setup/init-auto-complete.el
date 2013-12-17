;; AUTOCOMPLETE
; Load the default configuration
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")



(ac-flyspell-workaround)
(setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")

;; (global-auto-complete-mode t)
(setq
 ac-auto-show-menu t
 ac-dwim t
 ac-use-menu-map t
 ac-quick-help-delay 1
 ac-quick-help-height 60
 ac-auto-start 2
 ac-candidate-menu-min 2)

(define-key ac-menu-map (kbd "/") 'ac-isearch)
(define-key ac-menu-map (kbd "C-j") 'ac-next)
(define-key ac-menu-map (kbd "C-k") 'ac-previous)

;; don't ignore case
(setq ac-ignore-case 'smart)

(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-abbrev
               ac-source-yasnippet))

(dolist (mode '(log-edit-mode haml-mode sass-mode yaml-mode csv-mode espresso-mode
                haskell-mode html-mode nxml-mode sh-mode smarty-mode clojure-mode
                org-mode lisp-mode textile-mode tuareg-mode))
  (add-to-list 'ac-modes mode))

(add-hook 'comint-mode-hook 'auto-complete-mode)
