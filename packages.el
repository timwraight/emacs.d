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
	ace-jump-mode
        ack-and-a-half
        ack-menu
        auto-complete
        auto-indent-mode
        autopair
        centered-cursor-mode
        color-theme-solarized
        direx
        dockerfile-mode
        el-get
        emmet-mode
        expand-region
        flycheck
        flycheck-color-mode-line
        flyspell-lazy
        git-modes
        helm
        helm-ls-git
        helm-hoogle
        helm-git-grep
        icicles
        jedi
        jedi-direx
        js2-mode
        jss
        key-chord
        lalopmak-vimp
        less-css-mode
        magit
        markdown-mode
        multiple-cursors
        org-mode
        paredit
        pbcopy
        php-mode
        plantuml-mode
        powerline
        popwin
        pos-tip
        pymacs
        python-django
        rainbow-delimiters
        ssh-config-mode
        smex
        tagedit
        undo-tree
        vimp
        vimp-leader
        vimp-surround
        websocket
        yaml-mode
        yasnippet
        tim-zenburn
        ))

(el-get 'sync (append el-get-sources el-get-local-sources))
