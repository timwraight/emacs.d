(add-hook 'with-editor-mode-hook 'evil-insert-state)
(with-eval-after-load 'magit
(define-key magit-status-mode-map (kbd "e") 'next-line)
(define-key magit-status-mode-map (kbd "u") 'previous-line)
(define-key magit-status-mode-map (kbd "i") 'magit-stage)
(define-key magit-status-mode-map (kbd "n") 'magit-unstage)

(define-key magit-file-section-map (kbd "e") 'next-line)
(define-key magit-file-section-map (kbd "u") 'previous-line)
(define-key magit-file-section-map (kbd "i") 'magit-stage)
(define-key magit-file-section-map (kbd "n") 'magit-unstage)

(define-key magit-unstaged-section-map (kbd "e") 'next-line)
(define-key magit-unstaged-section-map (kbd "u") 'previous-line)
(define-key magit-unstaged-section-map (kbd "i") 'magit-stage)
(define-key magit-unstaged-section-map (kbd "n") 'magit-unstage)

(define-key magit-staged-section-map (kbd "e") 'next-line)
(define-key magit-staged-section-map (kbd "u") 'previous-line)
(define-key magit-staged-section-map (kbd "i") 'magit-stage)
(define-key magit-staged-section-map (kbd "n") 'magit-unstage)

(define-key magit-file-mode-map (kbd "e") 'next-line)
(define-key magit-file-mode-map (kbd "u") 'previous-line)
(define-key magit-file-mode-map (kbd "i") 'magit-stage)
(define-key magit-file-mode-map (kbd "n") 'magit-unstage)

(define-key magit-mode-map (kbd "e") 'next-line)
(define-key magit-mode-map (kbd "u") 'previous-line)
(define-key magit-mode-map (kbd "i") 'magit-stage)
(define-key magit-mode-map (kbd "n") 'magit-unstage)

(define-key magit-commit-section-map (kbd "e") 'next-line)
(define-key magit-commit-section-map (kbd "u") 'previous-line)
(define-key magit-commit-section-map (kbd "i") 'magit-stage)
(define-key magit-commit-section-map (kbd "n") 'magit-unstage)

(define-key magit-hunk-section-map (kbd "e") 'next-line)
(define-key magit-hunk-section-map (kbd "u") 'previous-line)
(define-key magit-hunk-section-map (kbd "i") 'magit-stage)
(define-key magit-hunk-section-map (kbd "n") 'magit-unstage)




)

(defun tw/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-upstream-remote)
                       "url"))
           (magit-get-current-branch))))


(defun tw/push-to-origin ()
  (interactive)
  (magit-run-git-async "push" "-v" "origin" "--set-upstream" (magit-get-current-branch)))
