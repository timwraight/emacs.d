(require 'helm-config)
(setq helm-ls-git-show-abs-or-relative 'relative)
(setq helm-ff-transformer-show-only-basename nil)

(setq helm-c-locate-command "mdfind -name %s %s")

(setq read-buffer-function 'ido-read-buffer)
(setq read-file-name-function 'ido-read-file-name)
(setq helm-full-frame nil)

(setq helm-display-function
      (lambda (buf)
        (switch-to-buffer buf)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))

(setq helm-truncate-lines t)

(require 'helm)
(require 'helm-org)
(require 'helm-files)
(define-key helm-map (kbd "M-e") 'helm-next-line)
(define-key helm-map (kbd "M-u") 'helm-previous-line)
(define-key helm-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-l") 'helm-previous-source)
(define-key helm-map (kbd "M-y") 'helm-next-source)
(define-key helm-generic-files-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-generic-files-map (kbd "M-/") 'helm-ff-run-grep)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "M-e") 'helm-next-line)
(define-key helm-find-files-map (kbd "M-u") 'helm-previous-line)
(define-key helm-find-files-map (kbd "M-n") 'helm-find-files-up-one-level)

;;; Jabber Contacts (jabber.el)
;; I think this is already defined in helm and can be deleted
(defun helm-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                  (cons (symbol-name item) item)) jids))))))


(defvar helm-c-source-jabber-contacts
    '((name . "Jabber Contacts")
      (init . (lambda () (require 'jabber)))
      (candidates . (lambda () (mapcar 'car (helm-c-jabber-online-contacts))))
      (action . (lambda (x)
                    (jabber-chat-with
                     (jabber-read-account)
                     (symbol-name
                      (cdr (assoc x (helm-c-jabber-online-contacts)))))))))

(defun helm-jabber-contacts ()
   "Preconfigured helm for ipython completions."
    (interactive)
    (delete-other-windows)
    (helm :sources helm-c-source-jabber-contacts
          :buffer "*helm jabber contacts*"))


(helm-autoresize-mode nil)

(require 'helm-org)
(require 'projectile)

(defvar tim-source-projectile-projects
  (helm-build-in-buffer-source "Projectile projects"
    :data (lambda ()
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects))
    :mode-line helm-ff-mode-line-string
    :action '(("Switch to project" .
               (lambda (project)
                 (let ((projectile-completion-system 'helm))
                   (projectile-switch-project-by-name project))))
              ("Open Dired in project's directory `C-d'" . dired)
              ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
              ("Grep in projects `M-s'.  With C-u, recurse" . helm-find-files-grep)
              ("Remove project(s) `M-D'" . helm-projectile-remove-known-project)))
  "Helm source for known projectile projects.")


(defun helm-timi ()
  "Like helm-mini, but for timi, geddit?"
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))

  (setq helm-projectile-sources-list helm-source-projectile-projects)
  (helm-other-buffer '(helm-source-buffers-list
                       helm-c-source-jabber-contacts
                       (helm-source-org-headings-for-files org-agenda-files)
                       tim-source-projectile-projects
                       helm-source-recentf)
                     "*helm timi*"))


(defun helm-proj ()
  "Like helm-mini, but for timi, geddit?"
  (interactive)
  (helm-other-buffer '(helm-source-ls-git-status
                       helm-source-ls-git
                       helm-source-git-grep)
                     "*helm proj*"))
