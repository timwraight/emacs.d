; INCLUDING OTHER INIT FILES
(server-start)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/")))


(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

(byte-recompile-directory "~/.emacs.d/")


(load-user-file "packages.el")
(load-user-file "interface.el")
(load-user-file "python.el")
(load-user-file "org.el")
(load-user-file "misc.el")
(load-user-file "editing.el")
(load-user-file "navigation.el")



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(debug-on-error nil)
 '(org-alphabetical-lists t)
 '(org-clock-into-drawer 2 t)
 '(org-global-properties (quote (("Effort_ALL" . "0 0:05 0:10 0:20 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00"))))
 '(python-shell-interpreter "ipython")
 '(safe-local-variable-values (quote ((pony-settings make-pony-project :python "/Users/twraight/Envs/dashboard/bin/python") (pony-settings make-pony-project :python "~/Envs/grace/bin/python" :settings "settings"))))
 '(scheme-program-name "guile")
 '(yaml-indent-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#042028" :foreground "#999999" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "Microsoft" :family "Consolas"))))
 '(cursor ((t (:background "#fffece" :foreground "#042028"))))
 '(ediff-current-diff-C ((t (:background "#393935"))))
 '(ediff-even-diff-A ((t (:background "#002a34"))))
 '(ediff-even-diff-B ((t (:background "#002a34"))))
 '(ediff-even-diff-C ((t (:background "#341d11"))))
 '(ediff-fine-diff-C ((t (:background "#624425"))))
 '(ediff-odd-diff-A ((t (:background "#00303c"))))
 '(ediff-odd-diff-B ((t (:background "#002a34"))))
 '(ediff-odd-diff-C ((t (:background "#343011"))))
 '(fixed-pitch ((t (:height 140 :family "Consolas"))))
 '(helm-selection ((t (:background "ForestGreen" :foreground "#1e1f17" :underline t))))
 '(link ((t (:foreground "#9d8464" :inverse-video nil :underline t :slant normal :weight normal))))
 '(magit-branch ((t (:inherit magit-header :foreground "#9c8d37"))))
 '(magit-header ((t (:inherit header-line :foreground "#00718c"))))
 '(magit-item-highlight ((t (:inherit highlight))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "#f19045"))) t)
 '(org-block ((t (:height 0.85 :family "Menlo"))))
 '(org-block-background ((t (:height 0.85 :family "Menlo"))))
 '(org-block-begin-line ((t (:foreground "#777" :height 0.85 :family "Menlo"))) t)
 '(org-block-end-line ((t (:inherit org-block-begin-line))) t)
 '(org-code ((t (:inherit shadow :family "Menlo"))))
 '(org-column ((t (:background "#00313d" :strike-through nil :underline nil :slant normal :weight normal :height 140 :family "Menlo"))))
 '(org-default ((t (:inherit nil :foreground "#96906a" :height 1.4 :family "Gill Sans"))))
 '(org-level-1 ((t (:inherit nil :foreground "#818f4e" :height 1.2))))
 '(org-level-2 ((t (:inherit nil :foreground "#856a6a" :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :foreground "#92805f"))))
 '(org-level-5 ((t (:inherit outline-5 :foreground "#0b5a8d"))))
 '(org-meta-line ((t (:inherit org-block-begin-line))))
 '(org-special-keyword ((t (:inherit font-lock-keyword-face :foreground "#003441"))))
 '(org-table ((t (:foreground "LightSkyBlue" :height 1 :family "Menlo"))))
 '(variable-pitch ((t (:foreground "#a0a19a" :height 1.2 :family "Lucida Grande"))))
 '(w3m-session-select ((t (:foreground "white" :family "Gill Sans"))) t))


(load-user-file "setup-specific.el")
