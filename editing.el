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

(setq truncate-partial-width-windows 80)
(setq fill-column 80)

;; UNDO TREE
(require 'undo-tree)

;; I can't remember ever having meant to use C-z to suspend the frame
(global-set-key (kbd "C-z") 'undo)

; Expand region
(require 'expand-region)
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
;; (global-set-key (kbd "M-<tab>") 'hippie-expand)
;; (delete 'try-expand-line hippie-expand-try-functions-list)
;; (delete 'try-expand-list hippie-expand-try-functions-list)

;;; turn off auto-fill in tex and markdown
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'latex-mode-hook 'turn-off-auto-fill)


;; SPELLING
;; (setq ispell-program-name "aspell"
;;       ispell-dictionary "english"
;;       ispell-dictionary-alist
;;       (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
;;                        ("-B" "-d" "english" "--dict-dir"
;;                         "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
;;                        nil iso-8859-1)))
;;         `((nil ,@default)
;;           ("english" ,@default))))
;; (setq ispell-extra-args '("--sug-mode=ultra"))
;; (setq ispell-personal-dictionary "~/.aspell.en.pws")
;; (setq flyspell-issue-message-flag nil)
;; (add-hook 'text-mode-hook 'turn-on-flyspell)


; FLYMAKE
(require 'flymake)

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


; use flymake cursor
(eval-after-load 'flymake '(require 'flymake-cursor))

;; Disable warning popups
(setq flymake-gui-warnings-enabled nil)

;; AUTOCOMPLETE

; Load the default configuration
(require 'popup)
(require 'auto-complete-config)
(ac-config-default)
(ac-ropemacs-initialize)
(add-to-list 'ac-sources 'ac-source-ropemacs)
(add-to-list 'ac-sources 'ac-source-yasnippet)
(add-to-list 'ac-sources 'ac-source-words-in-same-mode-buffers)
(add-to-list 'ac-sources 'ac-source-abbrev)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;; (setq-default ac-sources (add-to-list
;;                           'ac-sources
;;                           'ac-source-dictionary
;;                           'ac-source-filename
;;                           'ac-source-functions
;;                           'ac-source-yasnippet
;;                           'ac-source-variables
;;                           'ac-source-symbols
;;                           'ac-source-features
;;                           'ac-source-abbrev
;;                           'ac-source-words-in-same-mode-buffers
;;                           'ac-source-yasnippet))


;; PABBREV
;; (require 'pabbrev)


;; SAVE PLACE

(setq-default save-place t)

;; UNIX CONF FILES MODE
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))

;; GIT COMMIT MODE
(require 'git-commit-mode)
(require 'gitconfig-mode)
(require 'gitignore-mode)
(add-hook 'git-commit-mode-hook 'turn-on-flyspell)
(add-hook 'git-commit-mode-hook (lambda () (setq save-place 0)))
(setq magit-save-some-buffers 'dontask)


;; VIMP
(require 'vimp )
(vimp-mode 1)

(define-key vimp-insert-state-map "k" #'tim/maybe-exit)

(vimp-define-command tim/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

(require 'surround)
(global-surround-mode 1)

(global-rainbow-delimiters-mode 1)

;; COMMENTING

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c c") 'toggle-comment-on-line)
(eldoc-mode 1)
