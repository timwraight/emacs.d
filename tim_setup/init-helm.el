(require 'helm-config)
(setq helm-ls-git-show-abs-or-relative 'absolute)
(setq helm-ff-transformer-show-only-basename nil)

(setq
 helm-locate-command "mdfind 'kMDItemDisplayName == \"%s\" || kMDItemTextContent == \"%s\"'"
 helm-c-locate-command "mdfind 'kMDItemDisplayName == \"%s\" || kMDItemTextContent == \"%s\"'")

 (setq read-buffer-function 'ido-read-buffer)
(setq read-file-name-function 'ido-read-file-name)
(setq helm-full-frame nil)
(setq helm-echo-input-in-header-line t)
(setq helm-scroll-amount 8)

(setq helm-display-function
      (lambda (buf) (switch-to-buffer buf)))

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)))

(setq helm-truncate-lines nil)
(helm-mode)
(setq helm-M-x-fuzzy-match t)
(global-set-key "\M-x" 'helm-M-x)

(require 'helm)
(require 'helm-files)
(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
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
         (bcrumb-list (nreverse (org-get-outline-path t depth title)))
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
         (bcrumb-list (nreverse (org-get-outline-path t depth title)))
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

(defun helm-proj ()
  "Project searching, by tim."
  (interactive)
  (unless (and helm-source-ls-git-status
               helm-source-ls-git)
    (setq helm-source-ls-git-status
          (helm-make-source "Git status" 'helm-ls-git-status-source
            :fuzzy-match helm-ls-git-fuzzy-match)
          helm-source-ls-git
          (helm-make-source "Git files" 'helm-ls-git-source
            :fuzzy-match helm-ls-git-fuzzy-match)))
  (helm :sources '(helm-source-ls-git-status
                   helm-source-ls-git
                   helm-source-git-grep)
        ;; When `helm-ls-git-ls' is called from lisp
        ;; `default-directory' is normally let-bounded,
        ;; to some other value;
        ;; we now set this new let-bounded value local
        ;; to `helm-default-directory'.
        :default-directory default-directory
        :buffer "*helm proj*"))

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
                       helm-c-source-jabber-contacts
                       ;; helm-source-mantis-tickets
                       tim-source-projectile-projects
                       ;; helm-source-mu
                       helm-source-locate
                       helm-source-org-agenda-items
                       helm-source-recentf)
                     "*helm timi*"))


