(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-recipe-path 
      '("~/.emacs.d/custom/el-get/recipes/"
	"~/.emacs.d/el-get/el-get/recipes" 
	"~/.emacs.d/el-get/el-get/recipes/elpa/" 
	"~/.emacs.d/el-get/el-get/recipes/emacswiki/"))

(setq el-get-user-package-directory "~/.emacs.d/custom/el-get/setup/")

; Get the packages we only want locally.
(load-file "~/.emacs.d/custom/el-get/local-packages.el")

(setq el-get-sources
      '(
        ssh-config-mode
        ack-and-a-half
        ack-menu
        auto-complete
        autopair 
        color-theme-solarized
        direx
        el-get
        emacs-jabber
        emmet-mode
        flycheck 
        flycheck-color-mode-line
        flyspell-lazy 
        git-modes
        helm 
        helm-ls-git
        jedi
        jedi-direx
        js2-mode
        less-css-mode
        magit 
        markdown-mode
        org-mode
        paredit
        plantuml-mode
        pbcopy
        php-mode 
        python-django
        rainbow-delimiters 
        smex 
        tagedit 
        undo-tree 
        vimp
        vimp-leader
        vimp-surround
        yaml-mode 
        yasnippet 
        ))

(el-get 'sync (append el-get-sources el-get-local-sources))

