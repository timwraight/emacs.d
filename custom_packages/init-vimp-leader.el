; VIMP LEADER
(global-vimp-leader-mode)
(vimp-leader/set-leader "SPC")

(vimp-leader/set-key "'" (kbd "yse'"))
(vimp-leader/set-key ";" 'helm-mini)
(vimp-leader/set-key "=" 'balance-windows)
(vimp-leader/set-key "\"" (kbd "yse\""))
(vimp-leader/set-key "a" 'helm-git-grep-at-point)
(vimp-leader/set-key "d" 'helm-dash)
(vimp-leader/set-key "f" 'helm-find-files)
(vimp-leader/set-key "g" 'magit-status)
(vimp-leader/set-key "h" 'vc-version-ediff)
(vimp-leader/set-key "l" 'split-window-below)
(vimp-leader/set-key "o" 'helm-org-agenda-files-headings)
(vimp-leader/set-key "q" 'save-buffers-kill-emacs)
(vimp-leader/set-key "r" (lambda() (interactive) (kbd "ysiW")))
(vimp-leader/set-key "t" 'google-this)
(vimp-leader/set-key "z" (lambda () (interactive) (save-buffers-kill-terminal 1)))

;; Window keymap
(setq window-keymap (make-sparse-keymap))
(define-key window-keymap (kbd "i") 'vimp-window-right)
(define-key window-keymap (kbd "n") 'vimp-window-left)
(define-key window-keymap (kbd "u") 'vimp-window-up)
(define-key window-keymap (kbd "e") 'vimp-window-down)
(define-key window-keymap "1" 'delete-other-windows)
(define-key window-keymap "0" 'delete-window)
(define-key window-keymap "3" 'split-window-horizontally)
(define-key window-keymap "2" 'split-window-below)
(vimp-leader/set-key "w" window-keymap)

;; Checker keymap
(setq checker-keymap (make-sparse-keymap))
(define-key checker-keymap (kbd "n") 'flycheck-previous-error)
(define-key checker-keymap (kbd "i") 'flycheck-next-error)
(define-key checker-keymap (kbd "a") 'flyspell-auto-correct-previous-word)
(vimp-leader/set-key "c" checker-keymap)

;; Files keymap
(setq files-keymap (make-sparse-keymap))
(define-key files-keymap (kbd "r") 'helm-recentf)
(define-key files-keymap (kbd "s") 'save-buffer)
(define-key files-keymap (kbd "f") 'helm-find-files)
(define-key files-keymap (kbd "p") 'helm-browse-project)
(vimp-leader/set-key "f" files-keymap)

;; Evaluation keymap
(setq eval-keymap (make-sparse-keymap))
(define-key eval-keymap (kbd "x") 'eval-defun)
(define-key eval-keymap (kbd "b") 'eval-buffer)
(define-key eval-keymap (kbd "r") 'eval-region)

(vimp-leader/set-key "e" eval-keymap)



;; Help keymap

(setq help-keymap (make-sparse-keymap))
(define-key help-keymap (kbd "f") 'describe-function)
(define-key help-keymap (kbd "v") 'describe-variable)
(define-key help-keymap (kbd "a") 'helm-apropos)
(vimp-leader/set-key "h" help-keymap)

;; Project keymap
(setq project-keymap (make-sparse-keymap))
(define-key project-keymap (kbd "p") 'helm-projectile-switch-project)
(define-key project-keymap (kbd "f") 'helm-browse-project)

(vimp-leader/set-key "p" project-keymap)


(vimp-leader/set-key "b" 'ido-switch-buffer)
;; GLOBAL (windows and buffers)
(vimp-leader/set-key "r" 'helm-recentf)
(vimp-leader/set-key "k" 'kill-this-buffer)
(vimp-leader/set-key "s" 'save-buffer)
(vimp-leader/set-key "m" 'helm-jabber-contacts)
(vimp-leader/set-key "x" 'helm-M-x)
(vimp-leader/set-key "v" 'clipboard-yank)
(vimp-leader/set-key ";" 'split-window-vertically)
(vimp-leader/set-key ";" 'helm-mini)

(vimp-leader/set-key "i" 'forward-symbol)
(vimp-leader/set-key "n" (lambda () (interactive) (forward-symbol -1)))

;; AUTO-COMPLETE
;; Use kj to escape from auto-complete mode
;(key-chord-define ac-menu-map "lk" 'ac-stop)
;(key-chord-define ac-completing-map "lk" 'ac-stop)
;(key-chord-define ac-menu-map "ff" 'ac-isearch)

;; GLOBAL (org)
(vimp-leader/set-key "l" 'org-store-link)


(vimp-leader/set-key-for-mode 'sgml-mode "<right>" 'tagedit-forward-slurp-tag)
(vimp-leader/set-key-for-mode 'sgml-mode "<left>" 'tagedit-forward-barf-tag)
