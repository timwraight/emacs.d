(require 'cl)


;; Please don't load outdated byte code
(setq load-prefer-newer t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'bind-key)
;(require 'diminish)

(require 'subr-x)
(require 'rx)
(require 'time-date)

(use-package ns-win                     ; OS X window support
  :defer t
  :if (eq system-type 'darwin)
  :config
  (setq ns-pop-up-frames nil            ; Don't pop up new frames from the workspace
        ))

(require 'python)

(defvar tim-packages

  '(
    evil evil-leader evil-surround evil-mc
         company company-jedi company-quickhelp smartparens wgrep idris-mode helm-idris helm-mu haskell-mode
         direx yasnippet dockerfile-mode php-mode python-django tagedit helm-dash 
         helm-ls-git flycheck flycheck-pos-tip flycheck-color-mode-line s git-timemachine window-number
         magit markdown-mode python el-get helm evil-avy helm-ag emmet-mode py-isort swiper counsel
         buffer-move format-sql switch-window golden-ratio terraform-mode counsel-gtags ggtags
         outorg outshine navi-mode ivy-rich helm-gtags pytest web-mode sqlup-mode importmagic
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
(add-to-list 'load-path "~/.emacs.d/custom_packages/emacs-flycheck-mypy/")
(add-to-list 'load-path "~/.emacs.d/custom_packages/lalopmak-evil/")
(add-to-list 'load-path "~/.emacs.d/custom_packages/Pymacs/")

(load "~/.emacs.d/custom_packages/emacs-flycheck-mypy/flycheck-mypy.el")
(load "~/.emacs.d/custom_packages/init-flycheck-mypy.el")
(load "~/.emacs.d/custom_packages/crontab-mode.el")

(load "~/.emacs.d/custom_packages/bible_search/bibsearch.el")
(require 'lalopmak-evil)

;; Some setup to load after everything else, to overwrite other stuff
(load "~/.emacs.d/custom_packages/final-init.el")

;; extra packages which need to load later
(defvar extra-packages
  '()
  "A list of packages to ensure are installed at launch.")

(install-packages extra-packages)



(provide 'tim-packages)
;;; tim-packages.el ends here
