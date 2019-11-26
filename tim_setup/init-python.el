; Make sure to actually define the switch to monospaced function, or
;; the next bit won't work
;; do it in setup-specific so that you can use a font you actually have.
;; You can use code like this:
;;

(add-hook 'python-mode-hook 'buffer-switch-to-monospaced)
(add-hook 'inferior-python-mode-hook 'buffer-switch-to-monospaced)
(add-hook 'python-mode-hook 'electric-indent-local-mode)

(defun tw/isort-and-blacken ()
  (py-isort-buffer)
  (blacken-buffer)
  )

(setq tw/timlint-file-for-linting "file-for-linting.py")

(require 'request)
(defun tw/send-to-timlint ()
  (interactive)
  (message "Sending buffer for auto-formatting...")
  (let* ((original-buffer (current-buffer)))
    (request
     "http://127.0.0.1:5000"
     :type "POST"
     :data (json-encode `(
                          ("file_path" . ,(buffer-file-name))
                          ("virtualenv_root" . ,python-shell-virtualenv-root)
                           ))
     :headers '(("Content-Type" . "application/json"))
     ;; :data "key=value&key2=value2"  ; this is equivalent
     :parser 'buffer-string
     :sync nil
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (when data 
                   (message "Got some data")
                   (with-current-buffer tw/timlint-file-for-linting
                     (revert-buffer t t nil))
                   (replace-buffer-contents (find-file-noselect (concat "/tmp/" tw/timlint-file-for-linting)))
                   (message "Buffer updated!")))))))


(add-hook 'python-mode-hook
          (lambda () (add-hook 'before-save-hook 'tw/send-to-timlint nil 'local)))

(setq python-indent-offset 4)
;; (with-eval-after-load "vimp"
;;   (evil-define-key 'insert python-mode-map (kbd "M-h") 'rope-lucky-assist))

;; ;; Autofill inside of comments
;; (setq-mode-local python-mode
;;                  comment-auto-fill-only-comments t
;;                  comment-fill-column 99
;;                  )
(add-to-list 'auto-mode-alist '("\\.pyi\\'" . python-mode))


(add-hook 'python-mode-hook (lambda () (interactive) (setq comment-fill-column 99)))

;; (add-hook 'python-mode-hook (lambda () (interactive)
;;                               (if (buffer-file-name)
;;                                   (progn
;;                                     (hs-minor-mode)
;;                                     (hs-hide-all)))
;;                               ))

(defun send-to-pony-shell ()
  (interactive)
  (comint-send-string (get-buffer-process "*ponysh*") (concat (buffer-substring (region-beginning) (region-end)) "\n")))

(add-hook 'python-mode-hook (ggtags-mode 1))

(add-hook 'semantic-init-hook
          (lambda ()
            (interactive)
            (if (and (bound-and-true-p python-mode) (buffer-file-name))
                (helm-semantic nil))))

(defun tw/runserver ()
  (interactive)
  (let*
      ((default-directory (projectile-project-root)))
    (make-comint
     "Django runserver"
     (format "%s/bin/python" python-shell-virtualenv-root)
     nil
     (format "%ssrc/manage.py" (projectile-project-root))
     "runserver_plus"
     "--settings=octoenergy.settings_support"
     "8001"
     )))


(defun tw/makemigrations ()
  (interactive)
  (let*
      ((default-directory (projectile-project-root)))
    (switch-to-buffer (make-comint
     "Django migrations"
     (format "%s/bin/python" python-shell-virtualenv-root)
     nil
     (format "%ssrc/manage.py" (projectile-project-root))
     "makemigrations"
     "--settings=octoenergy.settings_support"
     ))))



(defun tw/trepanserver ()
  (interactive)
  (realgud:trepan3k
   (format "%ssrc/manage.py runserver_plus 8001 --settings=octoenergy.settings_support"
           (projectile-project-root))))

;; (use-package lsp-mode
;;   :ensure t
;;   :config

;;   ;; make sure we have lsp-imenu everywhere we have LSP
;;   (require 'lsp-ui)
;;   ;; (require 'lsp-imenu)
;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)  
;;   ;; get lsp-python-enable defined
;;   ;; NB: use either projectile-project-root or ffip-get-project-root-directory
;;   ;;     or any other function that can be used to find the root directory of a project
;;   (lsp-define-stdio-client lsp-python "python"
;;                            #'projectile-project-root
;;                            '("pyls"))

;;   ;; make sure this is activated when python-mode is activated
;;   ;; lsp-python-enable is created by macro above 
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (lsp-python-enable)))

;;   ;; lsp extras
;;   (use-package lsp-ui
;;     :ensure t
;;     :config
;;     (setq lsp-ui-sideline-ignore-duplicate t)
;;     (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;   (use-package company-lsp
;;     :config
;;     (push 'company-lsp company-backends))

;;   ;; NB: only required if you prefer flake8 instead of the default
;;   ;; send pyls config via lsp-after-initialize-hook -- harmless for
;;   ;; other servers due to pyls key, but would prefer only sending this
;;   ;; when pyls gets initialised (:initialize function in
;;   ;; lsp-define-stdio-client is invoked too early (before server
;;   ;; start)) -- cpbotha
;;   (defun lsp-set-cfg ()
;;     (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
;;       ;; TODO: check lsp--cur-workspace here to decide per server / project
;;       (lsp--set-configuration lsp-cfg)))

;;   (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))
