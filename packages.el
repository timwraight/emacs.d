(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(setq el-get-recipe-path 
      '("/Users/timwraight/.emacs.d/el-get/el-get/recipes" 
	"/Users/timwraight/.emacs.d/custom/el-get/recipes/" 
	"~/.emacs.d/el-get/el-get/recipes/elpa/" 
	"~/.emacs.d/el-get/el-get/recipes/emacswiki/"))

(setq el-get-user-package-directory "~/.emacs.d/custom/el-get/setup/")

(setq el-get-sources
      '(
	markdown
	ssh-config-mode
                                        ; haskell-mode
        ; jss 
        ack 
        auto-complete
        autopair 
        color-theme-solarized 
        el-get 
        emmet-mode
        flycheck 
        flyspell-lazy 
        git-modes
        helm 
        helm-ls-git
        js2-mode
        magit 
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

(el-get 'sync el-get-sources)
