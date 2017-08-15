; EVIL LEADER
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")

(evil-leader/set-key "'" (kbd "yse'"))
(evil-leader/set-key ";" 'helm-mini)
(evil-leader/set-key "=" 'balance-windows)
(evil-leader/set-key "\"" (kbd "yse\""))
(evil-leader/set-key "a" 'helm-git-grep-at-point)
(evil-leader/set-key "f" 'helm-find-files)
(evil-leader/set-key "h" 'vc-version-ediff)
(evil-leader/set-key "l" 'split-window-below)
(evil-leader/set-key "o" 'helm-org-agenda-files-headings)
(evil-leader/set-key "r" (lambda() (interactive) (kbd "ysiW")))
(evil-leader/set-key "t" 'google-this)
(require 'org)
(with-eval-after-load 'org
  (evil-leader/set-key (kbd "n") (lambda () (interactive) (find-file "~/org/journal.org"))))
(evil-leader/set-key "z" (lambda () (interactive) (suspend-tty)))

;; Window keymap
(defvar window-keymap (make-sparse-keymap)
  "Keymap for window commands.")
(defalias 'windows window-keymap)
(define-key window-keymap (kbd "i") 'evil-window-right)
(define-key window-keymap (kbd "n") 'evil-window-left)
(define-key window-keymap (kbd "u") 'evil-window-up)
(define-key window-keymap (kbd "e") 'evil-window-down)
(define-key window-keymap (kbd "t") 'transpose-windows)
(define-key window-keymap (kbd "f") 'fit-window-to-buffer)
(define-key window-keymap (kbd "o") 'other-window-kill-buffer)
(define-key window-keymap (kbd "SPC") (lambda () (interactive) (other-window -1)))

(define-key window-keymap "1" 'delete-other-windows)
(define-key window-keymap "0" (lambda () (interactive) (delete-window) (balance-windows)))
(define-key window-keymap "3" (lambda () (interactive) 
                                           (split-window-horizontally)
                                           (balance-windows)
                                                ))
(define-key window-keymap "2" 'split-window-below)
(define-key window-keymap "p" 'winner-undo)
(define-key window-keymap "\C-p" 'winner-redo)
(evil-leader/set-key "w" window-keymap)
(which-key-add-key-based-replacements
  "<SPC> w" "window commands")

(setq quit-keymap (make-sparse-keymap))
(define-key quit-keymap (kbd "q") 'save-buffers-kill-emacs)
(evil-leader/set-key "q" quit-keymap)
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

(evil-leader/set-key "s" elscreen-keymap)
(which-key-add-key-based-replacements
  "<SPC> s" "elscreen commands")


;; Org map
(setq org-keymap (make-sparse-keymap))
(define-key org-keymap (kbd "c") 'org-capture)
(define-key org-keymap (kbd "<SPC>") (lambda () (interactive) (org-capture nil "j")))
(define-key org-keymap (kbd "l") 'org-store-link)
(define-key org-keymap (kbd "n") 'org-narrow-to-subtree)
(define-key org-keymap (kbd "w") (lambda () (interactive) (find-file "~/org/journal.org")))

(evil-leader/set-key "o" org-keymap)
(which-key-add-key-based-replacements
  "<SPC> o" "org commands"
  "<SPC> o <SPC>" "Todo today")


;; Git map
(setq git-keymap (make-sparse-keymap))
(define-key git-keymap (kbd "<SPC>") 'magit-status)
(define-key git-keymap (kbd "b") 'magit-blame)
(define-key git-keymap (kbd "q") 'magit-blame-quit)
(define-key git-keymap (kbd "c") 'magit-blame-copy-hash)
(define-key git-keymap (kbd "t") 'git-timemachine)
(define-key git-keymap (kbd "n") 'git-timemachine-show-previous-revision)
(define-key git-keymap (kbd "i") 'git-timemachine-show-next-revision)
(define-key git-keymap (kbd "f") 'magit-commit-fixup)
(evil-leader/set-key "g" git-keymap)

;; Checker keymap
(setq checker-keymap (make-sparse-keymap))
(define-key checker-keymap (kbd "n") 'flycheck-previous-error)
(define-key checker-keymap (kbd "i") 'flycheck-next-error)
(define-key checker-keymap (kbd "u") 'previous-error)
(define-key checker-keymap (kbd "e") 'next-error)
(define-key checker-keymap (kbd "a") 'flyspell-auto-correct-previous-word)
(evil-leader/set-key "c" checker-keymap)
(which-key-add-key-based-replacements
  "<SPC> c" "checker commands")


;; Files keymap
(setq files-keymap (make-sparse-keymap))
(define-key files-keymap (kbd "r") 'helm-recentf)
(define-key files-keymap (kbd "s") 'save-buffer)
(define-key files-keymap (kbd "f") 'helm-find-files)
(define-key files-keymap (kbd "p") 'helm-browse-project)
(define-key files-keymap (kbd "SPC") 'save-buffer)

(evil-leader/set-key "f" files-keymap)
(which-key-add-key-based-replacements
  "<SPC> f" "file commands")


;; Evaluation keymap
(setq eval-keymap (make-sparse-keymap))
(define-key eval-keymap (kbd "x") 'eval-defun)
(define-key eval-keymap (kbd "SPC") 'eval-defun)
(define-key eval-keymap (kbd "b") 'eval-buffer)
(define-key eval-keymap (kbd "r") 'eval-region)
(evil-leader/set-key "e" eval-keymap)
(which-key-add-key-based-replacements
  "<SPC> e" "lisp evaluation commands")


;; Timp keymap
;; This is a keymap for common editing commands. We give it prominence on
;; the 't' key because these should be common operations to perform in normal mode
(setq timp-keymap (make-sparse-keymap))
(define-key timp-keymap (kbd "SPC") 'duplicate-line-or-region)
(define-key timp-keymap (kbd "m") 'helm-mark-ring)
(define-key timp-keymap (kbd "g") 'google-this)
(define-key timp-keymap (kbd "l") 'linum-mode)
(define-key timp-keymap (kbd "s") 'projectile-run-eshell)
(define-key timp-keymap (kbd "p") 'helm-projectile-switch-project)

(evil-leader/set-key "t" timp-keymap)
(which-key-add-key-based-replacements
  "<SPC> t" "common editing commands")


;; 'Like this' keymap
;; A keymap for editing other places in the buffer 'like this one' (my
;; excuse for the mnemonic)
(setq like-this-keymap (make-sparse-keymap))
(define-key like-this-keymap (kbd "SPC") 'evil-mc-make-cursor-here)
(define-key like-this-keymap (kbd "a") 'evil-mc-make-all-cursors)
(define-key like-this-keymap (kbd "l") 'evil-mc-mode)
(define-key like-this-keymap (kbd "u") 'evil-mc-undo-all-cursors)
(define-key like-this-keymap (kbd "c") 'evil-mc-make-cursor-here)
(define-key like-this-keymap (kbd "p") 'evil-mc-pause-cursors)
(define-key like-this-keymap (kbd "r") 'evil-mc-resume-cursors)
(define-key like-this-keymap (kbd "i") 'evil-mc-make-and-goto-next-match)
(define-key like-this-keymap (kbd "n") 'evil-mc-skip-and-goto-next-match)

(evil-leader/set-key"l" like-this-keymap)
(which-key-add-key-based-replacements "<SPC> l" "multiple cursors ")



;; Marks keymap
(setq marks-keymap (make-sparse-keymap))
(define-key marks-keymap (kbd "m") (lambda () (interactive) (push-mark)))
(define-key marks-keymap (kbd "SPC") 'helm-global-mark-ring)
(evil-leader/set-key "m" marks-keymap)
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
(evil-leader/set-key "h" help-keymap)
(which-key-add-key-based-replacements
  "<SPC> h" "help commands")


(defun pytest-last ()
  (interactive)
  (let*
      ((flags (concat pytest-cmd-flags " --lf")))
    (pytest-all flags))) 

(defun pytest-support ()
  (interactive)
  (setq pytest-cmd-flags (concat pytest-cmd-flags  " --support --ds=tests.settings_support"))
  (pytest-last))

(defun pytest-create-db ()
  (interactive)
  (setq pytest-cmd-flags (concat pytest-cmd-flags " --create-db"))
  (pytest-last)) 

;; Project keymap
(setq python-keymap (make-sparse-keymap))
(define-key python-keymap (kbd "i") 'rope-auto-import)
(define-key python-keymap (kbd "s") 'py-support)
(define-key python-keymap (kbd "f") 'rope-find-occurrences)
(define-key python-keymap (kbd "SPC") 'pytest-one)
(define-key python-keymap (kbd "1") 'pytest-one)
(define-key python-keymap (kbd "2") 'pytest-module)
(define-key python-keymap (kbd "3") 'pytest-all)
(define-key python-keymap (kbd "l") 'pytest-last)
(define-key python-keymap (kbd "c") 'pytest-create-db)



(evil-leader/set-key "p" python-keymap)
(which-key-add-key-based-replacements
  "<SPC> p" "python commands")
 
(evil-leader/set-key "<SPC>" 'evil-jump-backward)
(evil-leader/set-key "S-<SPC>" 'evil-jump-forward)



(evil-leader/set-key "b" 'ido-switch-buffer)
;; GLOBAL (windows and buffers)
(evil-leader/set-key "r" 'helm-recentf)
(evil-leader/set-key "k" 'kill-this-buffer)
(evil-leader/set-key "x" 'helm-M-x)
(evil-leader/set-key "v" 'clipboard-yank)
(evil-leader/set-key ";" 'split-window-vertically)
(evil-leader/set-key ";" 'helm-mini)

(evil-leader/set-key "i" 'forward-symbol)




(evil-leader/set-key-for-mode 'sgml-mode "<right>" 'tagedit-forward-slurp-tag)
(evil-leader/set-key-for-mode 'sgml-mode "<left>" 'tagedit-forward-barf-tag)



;; JEDI KEYMAP
(setq jedi-keymap (make-sparse-keymap))
(define-key jedi-keymap (kbd "g") 'jedi:goto-definition)
(define-key jedi-keymap (kbd "g") 'jedi:goto-definition)
(define-key jedi-keymap (kbd "d") 'jedi:show-doc)

(evil-leader/set-key-for-mode 'python-mode "j" jedi-keymap)

