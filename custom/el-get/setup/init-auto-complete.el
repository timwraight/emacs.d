;; AUTOCOMPLETE
; Load the default configuration
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")

(ac-flyspell-workaround)
(setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")

;; (global-auto-complete-mode t)
(setq
 ac-delay 0
 ac-auto-show-menu 0.1
 ac-use-fuzzy t
 ac-use-comphist t
 ac-dwim nil
 ac-use-menu-map t
 ac-quick-help-delay 1
 ac-quick-help-height 60
 ac-auto-start 3
 ac-ignore-case nil
 ac-candidate-limit nil
 ac-candidate-menu-min 2)

(define-key ac-menu-map (kbd "<RET>") 'ac-complete)
(define-key ac-menu-map (kbd "M-e") 'ac-next)
(define-key ac-menu-map (kbd "M-u") 'ac-previous)
(define-key ac-menu-map (kbd "M-i") 'ac-isearch)
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)


(set-default 'ac-sources
             '(ac-source-dictionary
               ac-source-words-in-buffer
               ac-source-words-in-same-mode-buffers
               ac-source-semantic
               ac-source-yasnippet))

(dolist (mode '(log-edit-mode haml-mode sass-mode yaml-mode csv-mode espresso-mode
                haskell-mode html-mode nxml-mode sh-mode smarty-mode clojure-mode jabber-chat-mode
                lisp-mode emacs-lisp-mode mu4e-compose-mode org-mode sql-mode textile-mode tuareg-mode))
  (add-to-list 'ac-modes mode))


(add-hook 'comint-mode-hook 'auto-complete-mode)
(add-hook 'org-mode-hook (lambda () (setq-local ac-auto-start 4)))
