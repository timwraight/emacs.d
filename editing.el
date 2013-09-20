; EDITING

(global-set-key (kbd "C-c C-c c") 'comment-region)
(global-set-key (kbd "C-c C-c u") 'uncomment-region)

(autopair-global-mode 1)
(setq autopair-autowrap t)

(global-auto-revert-mode)
(blink-cursor-mode -1)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq save-place-file "/tmp/emacs-places.txt")
(setq auto-save-visited-file-name t)

(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)
; sentences end in a single space
(setq sentence-end-double-space nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; tabs
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((max-column (or max 200))
         (tab-width (or width tab-width))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))

(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list (my-generate-tab-stops))


;;; turn off auto-fill in tex and markdown
(add-hook 'markdown-mode-hook 'turn-off-auto-fill)
(add-hook 'latex-mode-hook 'turn-off-auto-fill)


;; TEXT MODE EDITING
(vimp-leader/set-key-for-mode 'text-mode "p" 'flyspell-auto-correct-previous-word)

;; BACKUPS

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.saves"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups



;; SPELLING
(setq ispell-program-name "aspell"
      ispell-dictionary "british"
      ispell-dictionary-alist
      (let ((default '("[A-Za-z]" "[^A-Za-z]" "[']" nil
                       ("-B" "-d" "english" "--dict-dir"
                        "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
                       nil iso-8859-1)))
        `((nil ,@default)
          ("english" ,@default))))
(setq ispell-extra-args '("--sug-mode=ultra"))
(setq ispell-personal-dictionary "~/.aspell.en.pws")

;; SAVE PLACE
(setq-default save-place t)

;; UNIX CONF FILES MODE
(add-to-list 'auto-mode-alist '("\\.*rc$" . conf-unix-mode))


;; COMMENTING

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c c") 'toggle-comment-on-line)
(eldoc-mode 1)
