(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file "~/.emacs.d")))


(byte-recompile-directory "~/.emacs.d/")

(add-to-list 'load-path "~/.emacs.d/el-get/org-mode/lisp")

(load-user-file "packages.el")
(load-user-file "interface.el")
(load-user-file "navigation.el")
(load-user-file "editing.el")
(load-user-file "python.el")
(load-user-file "elisp.el")
(load-user-file "t-mode.el")
(load-user-file "misc.el")
(load-user-file "uk-holidays.el")

(setq custom-file "~/.emacs.d/setup-specific.el")
(load-user-file "setup-specific.el")
