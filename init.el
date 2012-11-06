<<<<<<< HEAD
;; INCLUDING OTHER INIT FILES
=======
; INCLUDING OTHER INIT FILES

>>>>>>> 960f4f4f20ba88eb5ef1938d48370dd8cf83440f
(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))


(load-user-file "packages.el")
(load-user-file "navigation.el")
(load-user-file "interface.el")
(load-user-file "editing.el")
(load-user-file "python.el")
(load-user-file "org.el")



;; SMEX  (IDO for meta-x)
(require 'smex)
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))



;; HTML
(require 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'pony-tpl-mode)
(add-hook 'html-mode-hook (lambda ()
                            (variable-pitch-mode 0)
                            (setq truncate-lines t)))


;; ReST mode
(add-hook 'rst-mode-hook (lambda ()
                           (variable-pitch-mode 0)))
(add-hook 'rst-adjust-hook 'rst-toc-update)

;; NYAN-MODE
(require 'nyan-mode)


;; HELP

(global-set-key (kbd "C-h a") 'apropos)

;; GIT

(global-set-key (kbd "C-x g") 'magit-status)
(require 'magit)

;; SEARCHING 

(define-key global-map "\C-x\C-r" 'rgrep)

;; MODES

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))


;; JAVASCRIPT
(autoload 'js-mode "js" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(require 'flymake-jshint)
(add-hook 'js-mode-hook
     (lambda () (flymake-mode t)))


;; CSS

(require 'flymake-csslint)


;; BROWSING

(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url) 
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)


;; MAIL

(setq gnus-select-method '(nnimap "wraight.net"
				  (nnimap-address "www43.a2hosting.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)))


;; PHP

(require 'php-mode)


;; TEX
(load "auctex.el" nil t t)
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode t)))


;; YAML
(require 'yaml-mode)
(setq yaml-indent-offset 2)


;; CLOJURE

(require 'clojure-mode)

;; *********************
;; EMACS-GENERATED STUFF
;; *********************


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(debug-on-error nil)
 '(org-agenda-files (quote ("~/Dropbox/org/general.org")))
 '(org-alphabetical-lists t)
 '(org-clock-into-drawer 2)
<<<<<<< HEAD
 '(org-global-properties (quote (("Effort_ALL" . "0 0:05 0:10 0:20 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))))
 '(safe-local-variable-values (quote ((pony-settings make-pony-project :python "~/Envs/grace/bin/python" :settings "settings"))))
=======
 '(org-global-properties (quote (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))))
 '(safe-local-variable-values (quote ((pony-settings make-pony-project :python "/Users/twraight/Envs/dashboard/bin/python" :settings "www/conf/local.py"))))
>>>>>>> 960f4f4f20ba88eb5ef1938d48370dd8cf83440f
 '(yaml-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#042028" :foreground "#999999" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Menlo"))))
 '(cursor ((t (:background "#fffece" :foreground "#042028"))))
 '(magit-branch ((t (:inherit magit-header :foreground "#9c8d37"))))
 '(magit-header ((t (:inherit header-line :foreground "#00718c"))))
 '(magit-item-highlight ((t (:inherit highlight :foreground "#9c8d37"))))
 '(org-block ((t (:height 0.85 :family "Menlo"))))
 '(org-block-background ((t (:height 0.85 :family "Menlo"))))
 '(org-block-begin-line ((t (:foreground "#777" :height 0.85 :family "Menlo"))) t)
 '(org-block-end-line ((t (:inherit org-block-begin-line))) t)
 '(org-code ((t (:inherit shadow :height 0.75 :family "Menlo"))))
 '(org-column ((t (:background "#00313d" :strike-through nil :underline nil :slant normal :weight normal :height 140 :family "Menlo"))))
 '(org-default ((t (:inherit nil :foreground "#96906a" :height 1.4 :family "Gill Sans"))))
 '(org-level-1 ((t (:inherit nil :foreground "#818f4e" :height 1.2))))
 '(org-level-2 ((t (:inherit nil :foreground "#856a6a" :height 1.1))))
 '(org-meta-line ((t (:inherit org-block-begin-line))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :foreground "#003441"))))
 '(org-table ((t (:foreground "LightSkyBlue" :height 1 :family "Menlo"))))
 '(variable-pitch ((t (:height 1.3 :family "Gill Sans"))))
 '(w3m-session-select ((t (:foreground "white" :family "Gill Sans")))))


