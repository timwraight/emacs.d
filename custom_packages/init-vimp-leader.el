; VIMP LEADER
(global-vimp-leader-mode)
(vimp-leader/set-leader "SPC")

(vimp-leader/set-key "'" (kbd "yse'"))
(vimp-leader/set-key ";" 'helm-mini)
(vimp-leader/set-key "=" 'balance-windows)
(vimp-leader/set-key "\"" (kbd "yse\""))
(vimp-leader/set-key "a" 'helm-git-grep-at-point)
(vimp-leader/set-key "f" 'helm-find-files)
(vimp-leader/set-key "g" 'magit-status)
(vimp-leader/set-key "h" 'vc-version-ediff)
(vimp-leader/set-key "l" 'split-window-below)
(vimp-leader/set-key "o" 'helm-org-agenda-files-headings)
(vimp-leader/set-key "r" (lambda() (interactive) (kbd "ysiW")))
(vimp-leader/set-key "t" 'google-this)
(vimp-leader/set-key "z" (lambda () (interactive) (save-buffers-kill-terminal 1)))

;; Window keymap
(defvar window-keymap (make-sparse-keymap)
  "Keymap for window commands.")
(defalias 'windows window-keymap)
(define-key window-keymap (kbd "i") 'vimp-window-right)
(define-key window-keymap (kbd "n") 'vimp-window-left)
(define-key window-keymap (kbd "u") 'vimp-window-up)
(define-key window-keymap (kbd "e") 'vimp-window-down)
(define-key window-keymap (kbd "t") 'transpose-windows)

(define-key window-keymap "1" 'delete-other-windows)
(define-key window-keymap "0" (lambda () (interactive) (delete-window) (balance-windows)))
(define-key window-keymap "3" (lambda () (interactive) 
                                           (split-window-horizontally)
                                           (balance-windows)
                                                ))
(define-key window-keymap "2" 'split-window-below)
(define-key window-keymap "p" 'winner-undo)
(define-key window-keymap "\C-p" 'winner-redo)
(vimp-leader/set-key "w" window-keymap)
(which-key-add-key-based-replacements
  "<SPC> w" "window commands")

(setq quit-keymap (make-sparse-keymap))
(define-key quit-keymap (kbd "q") 'save-buffers-kill-emacs)
(vimp-leader/set-key "q" quit-keymap)
(which-key-add-key-based-replacements
  "<SPC> q" "quit")


;; El-screen keymap
(setq elscreen-keymap (make-sparse-keymap))
(define-key elscreen-keymap (kbd "i") 'elscreen-next)
(define-key elscreen-keymap (kbd "n") 'elscreen-previous)
(define-key elscreen-keymap (kbd "c") 'elscreen-clone)
(define-key elscreen-keymap (kbd "k") 'elscreen-kill)
(define-key elscreen-keymap (kbd "r") 'elscreen-screen-nickname)
(define-key elscreen-keymap (kbd "g") 'elscreen-goto)
(define-key elscreen-keymap (kbd "t") 'elscreen-toggle)
(define-key elscreen-keymap (kbd "SPC") 'helm-elscreen-history)


(define-key elscreen-keymap (kbd "0") (lambda () (interactive) (elscreen-goto 0)))
(define-key elscreen-keymap (kbd "1") (lambda () (interactive) (elscreen-goto 1)))
(define-key elscreen-keymap (kbd "2") (lambda () (interactive) (elscreen-goto 2)))
(define-key elscreen-keymap (kbd "3") (lambda () (interactive) (elscreen-goto 3)))
(define-key elscreen-keymap (kbd "4") (lambda () (interactive) (elscreen-goto 4)))
(define-key elscreen-keymap (kbd "5") (lambda () (interactive) (elscreen-goto 5)))
(define-key elscreen-keymap (kbd "6") (lambda () (interactive) (elscreen-goto 6)))
(define-key elscreen-keymap (kbd "7") (lambda () (interactive) (elscreen-goto 7)))
(define-key elscreen-keymap (kbd "8") (lambda () (interactive) (elscreen-goto 8)))
(define-key elscreen-keymap (kbd "9") (lambda () (interactive) (elscreen-goto 9)))

(vimp-leader/set-key "s" elscreen-keymap)
(which-key-add-key-based-replacements
  "<SPC> s" "elscreen commands")


;; Org map
(setq org-keymap (make-sparse-keymap))
(define-key org-keymap (kbd "c") 'org-capture)
(define-key org-keymap (kbd "<SPC>") 'org-capture)

(vimp-leader/set-key "o" org-keymap)
(which-key-add-key-based-replacements
  "<SPC> o" "org commands")




;; Checker keymap
(setq checker-keymap (make-sparse-keymap))
(define-key checker-keymap (kbd "n") 'flycheck-previous-error)
(define-key checker-keymap (kbd "i") 'flycheck-next-error)
(define-key checker-keymap (kbd "u") 'previous-error)
(define-key checker-keymap (kbd "e") 'next-error)
(define-key checker-keymap (kbd "a") 'flyspell-auto-correct-previous-word)
(vimp-leader/set-key "c" checker-keymap)
(which-key-add-key-based-replacements
  "<SPC> c" "checker commands")


;; Files keymap
(setq files-keymap (make-sparse-keymap))
(define-key files-keymap (kbd "r") 'helm-recentf)
(define-key files-keymap (kbd "s") 'save-buffer)
(define-key files-keymap (kbd "f") 'helm-find-files)
(define-key files-keymap (kbd "p") 'helm-browse-project)
(define-key files-keymap (kbd "SPC") 'save-buffer)

(vimp-leader/set-key "f" files-keymap)
(which-key-add-key-based-replacements
  "<SPC> f" "file commands")


;; Evaluation keymap
(setq eval-keymap (make-sparse-keymap))
(define-key eval-keymap (kbd "x") 'eval-defun)
(define-key eval-keymap (kbd "SPC") 'eval-defun)
(define-key eval-keymap (kbd "b") 'eval-buffer)
(define-key eval-keymap (kbd "r") 'eval-region)
(vimp-leader/set-key "e" eval-keymap)
(which-key-add-key-based-replacements
  "<SPC> e" "lisp evaluation commands")


;; Timp keymap
;; This is a keymap for common editing commands. We give it prominence on
;; the 't' key because these should be common operations to perform in normal mode
(setq timp-keymap (make-sparse-keymap))
(define-key timp-keymap (kbd "SPC") 'duplicate-line-or-region)
(define-key timp-keymap (kbd "m") 'helm-mark-ring)
(define-key timp-keymap (kbd "l") 'linum-mode)

(vimp-leader/set-key "t" timp-keymap)
(which-key-add-key-based-replacements
  "<SPC> t" "common editing commands")


;; Marks keymap
(setq marks-keymap (make-sparse-keymap))
(define-key marks-keymap (kbd "m") (lambda () (interactive) (push-mark)))
(define-key marks-keymap (kbd "SPC") 'helm-global-mark-ring)
(vimp-leader/set-key "m" marks-keymap)
(which-key-add-key-based-replacements
  "<SPC> m" "marking commands")


;; Docs keymap
(setq help-keymap (make-sparse-keymap))
(define-key help-keymap (kbd "f") 'describe-function)
(define-key help-keymap (kbd "v") 'describe-variable)
(define-key help-keymap (kbd "a") 'helm-apropos)
(define-key help-keymap (kbd "SPC") 'describe-foo-at-point)
(define-key help-keymap (kbd "d") 'helm-dash)
(define-key help-keymap (kbd "k") 'describe-key)
(define-key help-keymap (kbd "s") 'helm-swoop-without-pre-input)

(define-key help-keymap "h" 'helm-dash-at-point)
(vimp-leader/set-key "h" help-keymap)
(which-key-add-key-based-replacements
  "<SPC> h" "help commands")

;; Project keymap
(setq python-keymap (make-sparse-keymap))
(define-key python-keymap (kbd "i") 'rope-auto-import)
(define-key python-keymap (kbd "SPC") 'pytest-one)
(define-key python-keymap (kbd "1") 'pytest-one)
(define-key python-keymap (kbd "2") 'pytest-module)
(define-key python-keymap (kbd "3") 'pytest-all)



(vimp-leader/set-key "p" python-keymap)
(which-key-add-key-based-replacements
  "<SPC> p" "python commands")
 
(vimp-leader/set-key "<SPC>" 'vimp-jump-backward)
(vimp-leader/set-key "S-<SPC>" 'vimp-jump-forward)



(vimp-leader/set-key "b" 'ido-switch-buffer)
;; GLOBAL (windows and buffers)
(vimp-leader/set-key "r" 'helm-recentf)
(vimp-leader/set-key "k" 'kill-this-buffer)
(vimp-leader/set-key "x" 'helm-M-x)
(vimp-leader/set-key "v" 'clipboard-yank)
(vimp-leader/set-key ";" 'split-window-vertically)
(vimp-leader/set-key ";" 'helm-mini)

(vimp-leader/set-key "i" 'forward-symbol)
(vimp-leader/set-key "n" (lambda () (interactive) (forward-symbol -1)))


;; GLOBAL (org)
(vimp-leader/set-key "l" 'org-store-link)


(vimp-leader/set-key-for-mode 'sgml-mode "<right>" 'tagedit-forward-slurp-tag)
(vimp-leader/set-key-for-mode 'sgml-mode "<left>" 'tagedit-forward-barf-tag)



;; JEDI KEYMAP
(setq jedi-keymap (make-sparse-keymap))
(define-key jedi-keymap (kbd "g") 'jedi:goto-definition)
(define-key jedi-keymap (kbd "g") 'jedi:goto-definition)
(define-key jedi-keymap (kbd "d") 'jedi:show-doc)

(vimp-leader/set-key-for-mode 'python-mode "j" jedi-keymap)

