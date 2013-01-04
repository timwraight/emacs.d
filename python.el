;; HOOKS

(add-hook 'python-mode-hook 
      (lambda ()
        (flymake-mode)
        (local-set-key [f2] 'flymake-goto-prev-error)
        (local-set-key [f3] 'flymake-goto-next-error)
        (setq truncate-lines 1)   
        ))


;; DOCS

(eval-when-compile (require 'pylookup))
(setq pylookup-program "~/.emacs.d/vendor/pylookup/pylookup.py")
(setq pylookup-db-file "~/.emacs.d/vendor/pylookup/pylookup.db")
(autoload 'pylookup-update "pylookup" 
  "Run pylookup-update and create the database at `pylookup-db-file'." t)
(global-set-key "\C-ch" 'pylookup-lookup)


;; PYMACS
(require 'pymacs)
;; avoid problems with pymacs (see http://pymacs.progiciels-bpi.ca/index.html)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(setq py-load-pymacs-p nil)

;; ROPEMACS

(pymacs-load "ropemacs" "rope-")
(setq ropemacs-guess-project t)
(setq ropemacs-separate-doc-buffer t)
(setq ropemacs-enable-autoimport nil)


;; DJANGO

(require 'pony-mode)
(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
            (pony-key (kbd "C-c C-p C-s") 'pony-south-schemamigration)
            (pony-key (kbd "C-c C-p C-a") 'pony-south-migrate)
	    (add-to-list 'ac-sources 'ac-source-ropemacs)))

(remove-hook 'python-mode-hook 'wisent-python-default-setup)

;; NOSETESTS
;; Don't need this right now, as I should just be able to do it from Pony-mode
;; (require 'nose)
;; (defvar nose-use-verbose nil) 
