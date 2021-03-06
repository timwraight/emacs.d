(require 'helm-config)
(setq helm-ls-git-show-abs-or-relative 'absolute)
(setq helm-ff-transformer-show-only-basename nil)
(with-eval-after-load 'helm
    (setq helm-display-function 'helm-default-display-buffer))

(setq
 helm-locate-command "mdfind 'kMDItemDisplayName == \"%s\" || kMDItemTextContent == \"%s\"'"
 helm-c-locate-command "mdfind 'kMDItemDisplayName == \"%s\" || kMDItemTextContent == \"%s\"'")

(setq helm-swoop-speed-or-color t)
 (setq read-buffer-function 'ido-read-buffer)
(setq read-file-name-function 'ido-read-file-name)
(setq helm-full-frame nil)
(setq helm-echo-input-in-header-line t)
(setq helm-scroll-amount 8)


(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))

(setq helm-follow-mode-persistent t)
(setq helm-truncate-lines nil)
(helm-mode)
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-copy))
(add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename))
(add-to-list 'helm-completing-read-handlers-alist '(projectile-switch-to-buffer . ido))
(setq helm-M-x-fuzzy-match t)

(require 'helm)
(require 'helm-files)
(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
(define-key helm-map (kbd "M-e") 'helm-next-line)
(define-key helm-map (kbd "M-u") 'helm-previous-line)
(define-key helm-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-l") 'helm-previous-source)
(define-key helm-map (kbd "M-y") 'helm-next-source)
(define-key helm-map (kbd "M-v") 'yank)
(define-key helm-generic-files-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-generic-files-map (kbd "M-/") 'helm-ff-run-grep)
(define-key helm-generic-files-map (kbd "M-RET") 'helm-ff-run-switch-other-window)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key helm-find-files-map (kbd "M-i") 'helm-execute-persistent-action)
(define-key helm-find-files-map (kbd "M-e") 'helm-next-line)
(define-key helm-find-files-map (kbd "M-u") 'helm-previous-line)
(define-key helm-find-files-map (kbd "M-n") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "M-n") 'helm-find-files-up-one-level)
(define-key helm-read-file-map (kbd "M-i") 'helm-execute-persistent-action)

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


(setq helm-autoresize-mode nil)

(require 'projectile)

(defvar tim-source-projectile-projects
  (helm-build-in-buffer-source "Projectile projects"
    :data (lambda ()
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects))
    :mode-line helm-read-file-name-mode-line-string
    :action '(("Switch to project" .
               (lambda (project)
                 (let ((projectile-completion-system 'helm))
                   (projectile-switch-project-by-name project))))
              ("Open Dired in project's directory `C-d'" . dired)
              ("Open project root in vc-dir or magit `M-g'" . helm-projectile-vc)
              ("Grep in projects `M-s'.  With C-u, recurse" . helm-find-files-grep)
              ("Remove project(s) `M-D'" . helm-projectile-remove-known-project)))
  "Helm source for known projectile projects.")


(defun element-and-ancestors ()
    "Given an element at point, return a cons of its ancestry string (including filename)
     and marker position"
  (let* ((element (org-element-at-point))
         (title (org-element-property :title element))
         (depth (org-element-property :level element))
         (todo-keyword (org-element-property :todo-keyword element))
         (bcrumb-list (nreverse (org-get-outline-path t t)))
         (bcrumb-string (s-join " -> " (delq nil (cons todo-keyword (cons (buffer-name) (nreverse (cons title bcrumb-list))))))))
    (cons bcrumb-string (point-marker)))
  )

; should merge this with function above and just let it take an argument to
; decide whether to include the filename in the string, but need to find out
; how to compose functions, as I think helm functions need to take no arguments
(defun element-and-ancestors-without-file ()
    "Given an element at point, return a cons of its ancestry string (EXcluding filename)
     and marker position"
  (let* ((element (org-element-at-point))
         (title (org-element-property :title element))
         (depth (org-element-property :level element))
         (todo-keyword (org-element-property :todo-keyword element))
         (bcrumb-list (nreverse (org-get-outline-path t t)))
         (bcrumb-string (s-join " -> " (delq nil (cons todo-keyword (nreverse (cons title bcrumb-list)))))))
    (cons bcrumb-string (point-marker)))
  )
  
(defun org-get-agenda-items ()
  (org-map-entries 'element-and-ancestors nil 'agenda))


(defvar helm-source-agenda-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-a")           'helm-org-archive-item)
    (define-key map (kbd "M-r")           'helm-refile)
    (define-key map (kbd "M-t")           'helm-change-state)
    (define-key map (kbd "M-c")           'helm-clock-in)
    (delq nil map))
  "Keymap for `helm-source-agenda-items'.")

(defun helm-org-archive-item ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-org-heading-archive)))

(defun helm-refile ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-org-heading-refile)))

(defun helm-clock-in ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-org-heading-clock-in)))

(defun helm-change-state ()
  (interactive)
  (with-helm-alive-p
    (helm-attrset 'change-state '(helm-org-heading-change-state . never-split))
    (helm-execute-persistent-action 'change-state)))


(defvar helm-source-org-agenda-items
  '((name . "Agenda Items")
    (candidates . org-get-agenda-items)
    (persistent-action . helm-org-heading-clock-in)
    (action . (("Go to line" . helm-org-goto-marker)
               ("Clock in on this heading" . helm-org-heading-clock-in)
               ("Archive this heading" . helm-org-heading-archive)
               ("Refile to this heading" . helm-org-heading-refile)
               ("Change state of this heading" . helm-org-heading-change-state)
               ("Insert link to this heading"
               . helm-org-insert-link-to-heading-at-marker)))))


(defun helm-org-heading-clock-in (marker)
  (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (org-clock-in)))

(defun helm-org-heading-archive (marker)
  (interactive)
  (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (org-archive-subtree)))

(defun helm-org-heading-change-state (marker)
  (interactive)
  (with-current-buffer (marker-buffer marker)
                       (goto-char (marker-position marker))
                       (org-todo)))

(defun helm-org-agenda-items ()
  (interactive)
  (helm :sources 'helm-source-org-agenda-items
        :keymap helm-source-agenda-keymap))


(require 'soap-client)
;; (setq mantis-wsdl (soap-load-wsdl-from-url "https://tracker.tangentlabs.co.uk:443/api/soap/mantisconnect.php?wsdl"))
(defun mantis-get-issues ()
  (car (soap-invoke mantis-wsdl
                    "MantisConnectPort"
                    "mc_project_get_issue_headers"
                    mantis-username
                    mantis-password
                    538             ; project_id
                    1               ; page_number
                    50        ; per_page
                    )))

;; (setq mantis-tickets (mapcar 'mantis-format-ticket (mantis-get-issues)))
;; (print mantis-tickets)

(defun mantis-format-ticket (ticket)
    "Given a TICKET, return a a formatted string suitable for display."
    (let ((title (cdr (assoc 'summary ticket)))
          (last-updated (cdr (assoc 'last_updated ticket)))
          (status (cdr (assoc 'status ticket)))
          (assigned-to (cdr (assoc 'handler ticket)))
          (ticket-id (cdr (assoc 'id ticket)))
          
           )
      (cons (format "%s" title) . ticket-id)))

;; (defvar helm-source-mantis-tickets
;;   '((name . "Mantis Tickets")
;;     (candidates . mantis-get-issues)
;;     (persistent-action . helm-org-heading-clock-in)
;;     (action . (("Go to line" . helm-org-goto-marker)
;;                ("Clock in on this heading" . helm-org-heading-clock-in)
;;                ("Archive this heading" . helm-org-heading-archive)
;;                ("Refile to this heading" . helm-org-heading-refile)
;;                ("Change state of this heading" . helm-org-heading-change-state)
;;                ("Insert link to this heading"
;;                . helm-org-insert-link-to-heading-at-marker)))))
(require 'helm-ag)

(add-hook 'helm-major-mode-hook 'buffer-switch-to-monospaced)

(defun helm-proj ()
  "Project searching, by tim."
  (interactive)
  (unless helm-source-ls-git
    (setq helm-source-ls-git
          (helm-make-source "Git files" 'helm-ls-git-source
            :fuzzy-match helm-ls-git-fuzzy-match
            :requires-pattern 2)))
  (setq helm-ag--default-directory (helm-ag--project-root))
  (helm :sources '(helm-source-ls-git
                   helm-source-do-ag)
        ;; When `helm-ls-git-ls' is called from lisp `default-directory' is normally let-bounded,
        ;; to some other value; we now set this new let-bounded value local to
        ;; `helm-default-directory'.
        :default-directory default-directory
        :buffer "*helm proj*"))


(defun tw/git-files ()
  (interactive)
  (unless helm-source-ls-git
    (setq helm-source-ls-git
          (helm-make-source "Git files" 'helm-ls-git-source
            :fuzzy-match helm-ls-git-fuzzy-match
            :requires-pattern 2)))
  (helm :sources '(helm-source-ls-git)
        ;; When `helm-ls-git-ls' is called from lisp `default-directory' is normally let-bounded,
        ;; to some other value; we now set this new let-bounded value local to
        ;; `helm-default-directory'.
        :default-directory default-directory
        :buffer "*helm git files*"))


(defun tw/helm-ag ()
  "Project searching, by tim."
  (interactive)
  (setq helm-ag--default-directory (helm-ag--project-root))
  (helm :sources '(helm-source-do-ag)
        :default-directory default-directory
        :buffer "*helm ag results*"))


(with-eval-after-load 'mu4e
  (require 'helm-mu)
  )


(defun helm-timi ()
  "Like helm-mini, but for timi, geddit?"
  (interactive)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm-other-buffer '(helm-source-buffers-list
                       ;; helm-c-source-jabber-contacts
                       ;; helm-source-mantis-tickets
                       tim-source-projectile-projects
                       ;; helm-source-mu
                       helm-source-locate
                       helm-source-org-agenda-items
                       helm-source-recentf)
                     "*helm timi*"))


;; ripgrep

(setq helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")


;; FZF

(defvar helm-fzf-source
  (helm-build-async-source "fzf"
    :candidates-process 'helm-fzf--do-candidate-process
    :nohighlight t
    :requires-pattern 2
    :candidate-number-limit 9999))

(defun helm-fzf--do-candidate-process ()
  (let* ((cmd-args `("fzf" "-x" "-f" ,helm-pattern))
         (proc (apply #'start-file-process "helm-fzf" nil cmd-args)))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

(defun helm-fzf (directory)
  (interactive "D")
  (let ((default-directory directory))
    (helm :sources '(helm-fzf-source)
:buffer "*helm-fzf*")))


(defun tw/get-recent-branches ()
  (split-string
   (shell-command-to-string "git reflog --since '1 month ago' | grep checkout | awk '{ print $NF }' | head -n 100 |  nl | sort -u -k 2,2 | sort -k 1,1 | grep -v '\\\\^' | awk '{ print $2 }' | head -n 20")))

(defun tw/new-git-branch ()
  (interactive)
  (let*
      ((branch-name (read-string "New branch name: ")))
    (shell-command-to-string (concat "git checkout -b " branch-name))
      )
  )

(defun helm-git-branches ()
  (interactive)
    (helm :sources (helm-build-sync-source "Recent git branches"
                     :candidates 'tw/get-recent-branches
                     :nohighlight t
                     :candidate-number-limit 9999
                     :action (lambda (candidate)
                               (magit-checkout candidate)))
          :buffer "*helm-git-recent-branches*"))

(defun tw/diff-current-file-with-master ()
  (interactive)
    (vc-version-ediff (cons (buffer-file-name) ()) "master" "HEAD"))

(defun helm-curated-actions ()
  (interactive)
    (helm :sources (helm-build-sync-source "Useful actions"
                     :candidates '(git-rebase-onto-master
                                   tw/visit-pull-request-url
                                   tw/get-recent-branches
                                   tw/new-git-branch
                                   tw/diff-current-file-with-master)
                     :nohighlight t
                     :candidate-number-limit 9999
                     :action (lambda (fname) (funcall (intern fname))))
          :buffer "*helm-useful-actions*"))

;; (global-set-key (kbd "M-,") 'helm-global-mark-ring)
;; (evil-global-set-key 'normal (kbd "M-,") 'helm-global-mark-ring)
