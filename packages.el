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
	context-export
        el-get
        emmet-mode
        flycheck 
        flyspell-lazy 
        git-modes
        haskell-mode
        helm 
        helm-ls-git
        js2-mode
        magit 
        markdown-mode
        org-mode
        paredit
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

