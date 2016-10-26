; EDITING

;; TERMINAL
;; Get shift-up working from terminal

(define-key input-decode-map "\e[1;2A" [S-up])
(define-key input-decode-map "\e[1;6A" [C-S-up])

(if (equal "xterm-256color" (tty-type))
      (define-key input-decode-map "\e[1;2A" [S-up])
      (define-key input-decode-map "\e[1;6A" [C-S-up]))


(global-set-key (kbd "C-c C-c c") 'comment-region)
(global-set-key (kbd "C-c C-c u") 'uncomment-region)

;; JAVASCRIPT
(add-hook 'javascript-mode-hook (lambda ()
 				  (define-key js-mode-map (kbd "<M-RET>") 'auto-indent-eol-char-newline)
				  (define-key js-mode-map (kbd "<C-RET>") 'auto-indent-eol-newline)
				  (key-chord-define js-mode-map ",/" 'helm-javascript-functions)))



;; Copy yanks to clipboard
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)


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


;; BACKUPS

(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist `((".*" . ,temporary-file-directory))
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   undo-tree-history-directory-alist `(("." . "~/.saves/undo-tree"))
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
(setq-default require-final-newline t)

;; COMMENTING

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-c c") 'toggle-comment-on-line)
(eldoc-mode 1)


;; copy line
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))