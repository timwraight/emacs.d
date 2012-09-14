;; PACKAGES

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))


(add-to-list 'load-path "~/.emacs.d/vendor/")
(let ((default-directory "~/.emacs.d/vendor/"))
  (normal-top-level-add-subdirs-to-load-path))

;; NAVIGATION
(windmove-default-keybindings) 
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1))) ;; back one
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2))) ;; forward two
(setq windmove-wrap-around t)

;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;; FILES

;; From https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
	(filename (buffer-file-name)))
    (if (not filename)
	(message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
	(progn 	 (rename-file name new-name 1) 	 (rename-buffer new-name) 	 (set-visited-file-name new-name) 	 (set-buffer-modified-p nil)))))) 

;; Also from https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
	 (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
    
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn 	(copy-file filename newname 1) 	(delete-file filename) 	(set-visited-file-name newname) 	(set-buffer-modified-p nil) 	t)))) 



;; RECENT FILES
(recentf-mode t)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(setq recentf-max-saved-items 50)

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))


(winner-mode 1)
(global-set-key (kbd "C-c <up>") 'winner-undo)
(global-set-key (kbd "C-c <down>") 'winner-redo)


;; IDO MODE

(ido-mode t)
(setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point nil
        ido-max-prospects 10)


;; SMEX  (IDO for meta-x)
(require 'smex)
(global-set-key [(meta x)] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))


;; EDITING
;; I can't remember ever having meant to use C-z to suspend the frame
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-c C-c c") 'comment-region)
(global-set-key (kbd "C-c C-c u") 'uncomment-region)
(setq mac-right-option-modifier nil)
(setq mac-right-command-modifier 'meta)

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
(global-auto-revert-mode)
(blink-cursor-mode -1)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq save-place-file "/tmp/emacs-places.txt")

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
; sentences end in a single space
(setq sentence-end-double-space nil)
(global-set-key (kbd "RET") 'newline-and-indent)

(setq truncate-partial-width-windows 80)
(setq fill-column 80)


;; I can't remember ever having meant to use C-z to suspend the frame
(global-set-key (kbd "C-z") 'undo)

; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

; tabs
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(setq tab-width 4)
(setq tab-stop-list (my-generate-tab-stops))



;; SNIPPETS
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/vendor/yasnippet/snippets")
(yas-global-mode t)
(global-set-key (kbd "M-<tab>") 'hippie-expand)
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; INTERFACE
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(setq max-mini-window-height 1)
(setq inhibit-splash-screen t)
(tooltip-mode -1)
(auto-compression-mode t)
(global-font-lock-mode t)
(show-paren-mode 1)
;; default to unified diffs
(setq diff-switches "-u")

; Use C-x C-m for M-x
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)

(require 'rainbow-delimiters)
(rainbow-delimiters-mode 1)

;; TYPOGRAPHY

(setq line-spacing 4)
(global-visual-line-mode t)

;; Turn on auto-fill (ie, break lines after 80 chars or whatever) globally
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;; turn off auto-fill in tex and markdown
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'latex-mode-hook 'turn-off-auto-fill)


;; SPELLING
(setq ispell-program-name "aspell"
      ispell-dictionary "english"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-personal-dictionary "~/.aspell.en.pws")
(setq flyspell-issue-message-flag nil)
(add-hook 'text-mode-hook 'turn-on-flyspell)


; FLYMAKE

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" flymake-pyflakes-init))
   (delete '("\\.html?\\'" flymake-xml-init)
           flymake-allowed-file-name-masks))


;; Disable warning popups
(setq flymake-gui-warnings-enabled nil)


;; PYTHON

(add-hook 'python-mode-hook 
      (lambda () 
        (unless (eq buffer-file-name nil) (flymake-mode 1)) ;dont invoke flymake on temporary buffers for the interpreter
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


;; DJANGO

(require 'pony-mode)

;; HTML
(require 'zencoding-mode)
(add-hook 'html-mode-hook 'zencoding-mode)
(add-hook 'html-mode-hook 'pony-tpl-mode)


;; SOLARIZED 

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)


;; ORG MODE
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(require 'org-install)
(setq org-default-notes-file "~/Dropbox/org/general.org")
(global-set-key (kbd "<f12>") 'org-agenda-list)
(global-set-key (kbd "<f11>") 'org-capture)
(setq org-startup-indented 1)
(setq org-use-speed-commands t)
(add-hook 'org-mode-hook (lambda ()
                           (variable-pitch-mode t)
                           ()))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit 1)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

; Targets include this file and any file contributing to the agenda -
; up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)


; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/general.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("a" "Question" entry (file+headline "~/Dropbox/org/general.org" "Questions")
         "*  %?\n %i\n  %a")))

;;;;; Columns
(setq org-global-properties )
(setq org-columns-default-format " %25ITEM %TODO %17Effort(Estimated Effort){:} %CLOCKSUM")

;; org-babel

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   (sql . t)
   (ditaa . t)
   ))

(setq org-ditaa-jar-path "~/bin/ditaa.jar")

;; HELP

(global-set-key (kbd "C-h a") 'apropos)

;; GIT

(define-key global-map "\M-\C-g" 'magit-status)


;; SEARCHING 

(define-key global-map "\C-x\C-r" 'rgrep)

;; MODES

(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

;; AUTOCOMPLETE

; Load the default configuration
(require 'popup)
(require 'auto-complete)
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)
;(add-to-list 'ac-sources 'ac-source-yasnippet)


;; JAVASCRIPT
(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))



;; BROWSING

(require 'w3m-load)
(setq browse-url-browser-function 'w3m-browse-url) 
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)


;; MAIL

(setq gnus-select-method '(nnimap "gmail"
				  (nnimap-address "imap.gmail.com")
				  (nnimap-server-port 993)
				  (nnimap-stream ssl)))


;; PHP

(require 'php-mode)


;; TEX
(load "auctex.el" nil t t)
(add-hook 'text-mode-hook (lambda () (variable-pitch-mode t)))


;; YAML

(setq yaml-indent-offset 4)


;; *********************
;; EMACS-GENERATED STUFF
;; *********************


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(debug-on-error t)
 '(org-agenda-files (quote ("~/Dropbox/org/general.org")))
 '(org-global-properties (quote (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))))
 '(safe-local-variable-values (quote ((pony-settings make-pony-project :python "/Users/twraight/Envs/dashboard/bin/python" :settings "www/conf/local.py") (pony-settings make-pony-project :python "~/Envs/grace/bin/python" :settings "settings") (pony-settings make-pony-project :python "/Users/tim/Envs/grace/bin/python" :settings "settings") (pony-settings make-pony-project :python "/Users/tim/.virtualenvs/grace/bin/python" :settings "settings")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#042028" :foreground "#708183" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "apple" :family "Menlo"))))
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
 '(w3m-session-select ((t (:foreground "white" :family "Gill Sans"))) t))


