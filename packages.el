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
	org-mode ; load first to avoid getting built-in org
        js2-mode ; similar
        ssh-config-mode
	helm
	vimp
        ace-jump-mode
        auto-complete
        auto-indent-mode
        color-theme-solarized
        direx
        dockerfile-mode
        el-get
        emacs-jabber
        emmet-mode
        flycheck
        flycheck-color-mode-line
        flyspell-lazy
        git-modes
        ;; git-timemachine
        helm
        helm-etags-plus
        helm-ls-git
        helm-hoogle
        helm-git-grep
        jedi
        jedi-direx
        jss
        json-mode  ; has really nice beautifier
        key-chord
        lalopmak-vimp
        less-css-mode
        magit
        markdown-mode
        multiple-cursors
        nix-mode
        paredit
        pbcopy
        php-mode
        projectile
        ;; plantuml-mode
        ;; powerline
        popup
        popwin
        pos-tip
        python-django
        rainbow-delimiters
        ssh-config-mode
        smartparens
        smex
        tagedit
        undo-tree
        vimp-leader
        vimp-surround
        websocket
        wgrep ; excellent! turns grep buffers into find/replace buffers
        yaml-mode
        yasnippet
        tim-zenburn
        ))

(el-get 'sync (append el-get-sources el-get-local-sources))
