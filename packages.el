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
        el-get 
        color-theme-solarized 
        helm 
        ack 
        pbcopy
        undo-tree 
        yasnippet 
        flyspell-lazy 
        flycheck 
        auto-complete
        rainbow-delimiters 
        autopair 
        paredit
        git-modes
        smex 
        tagedit 
        magit 
        js2-mode
        ; jss 
        php-mode 
        yaml-mode 
                                        ; haskell-mode
        vimp
        vimp-leader
        vimp-surround
        helm-ls-git
        emmet-mode
	markdown
	ssh-config-mode
        ))

(el-get 'sync el-get-sources)
