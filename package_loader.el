(require 'cl)

(defvar tim-packages
  '(
    company company-jedi company-quickhelp smartparens wgrep idris-mode helm-idris helm-mu
    direx jedi-direx yasnippet dockerfile-mode php-mode python-django tagedit
    helm-ls-git helm-git-grep flycheck flycheck-pos-tip flycheck-color-mode-line s git-timemachine
    rainbow-delimiters projectile helm-projectile jabber jabber-otr alert
    undo-tree volatile-highlights yaml-mode zenburn-theme
    magit magit-gh-pulls markdown-mode python el-get helm
    )
  "A list of packages to ensure are installed at launch.")

(defun install-packages (package-list)
  (defun tim-packages-installed-p ()
    (loop for p in tim-packages
          when (not (package-installed-p p)) do (return nil)
          finally (return t)))

  (unless (tim-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Refreshing our packages...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p tim-packages)
      (when (not (package-installed-p p))
        (package-install p))))
  )


(install-packages tim-packages)

;; Do setup for my packages
(defun load-all-in-directory (dir)
"`load' all elisp libraries in directory DIR which are not already loaded."
(interactive "D")
(let ((libraries-loaded (mapcar #'file-name-sans-extension
				(delq nil (mapcar #'car load-history)))))
  (dolist (file (directory-files dir t ".+\\.elc?$"))
    (let ((library (file-name-sans-extension file)))
      (unless (member library libraries-loaded)
	(load library nil t)
	          (push library libraries-loaded))))))

(load-all-in-directory "~/.emacs.d/tim_setup")


;; Get my custom packages
(add-to-list 'load-path "~/.emacs.d/custom_packages/mu4e/")
(add-to-list 'load-path "~/.emacs.d/custom_packages/vimp/")
(add-to-list 'load-path "~/.emacs.d/custom_packages/vimp-leader/")
(add-to-list 'load-path "~/.emacs.d/custom_packages/vimp-surround/")
(add-to-list 'load-path "~/.emacs.d/custom_packages/lalopmak-vimp/")
(require 'vimp)
(require 'mu4e)
(require 'vimp-leader)
(load "~/.emacs.d/custom_packages/init-vimp.el")
(load "~/.emacs.d/custom_packages/init-vimp-leader.el")
(load "~/.emacs.d/custom_packages/init-vimp-surround.el")
(load "~/.emacs.d/custom_packages/init-mu4e.el")
(require 'lalopmak-vimp)


;; extra packages which need to load later
(defvar extra-packages
  '(helm_mu)
  "A list of packages to ensure are installed at launch.")

(install-packages extra-packages)



(provide 'tim-packages)
;;; tim-packages.el ends here