(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
        (eval-print-last-sexp)))

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
        ; a few packages we load first so that we can reference them in our
        ; init-x files, without having to list them as explicit dependencies
        ; (and thus create a separate recipe for them)
        s
        perspective

        ; ones to load first so as to override built in versions
        org-mode
        js2-mode
        ssh-config-mode
        helm
        git-timemachine
        vimp

        ; normal packages
        alert
        ace-jump-mode
        auto-complete
        auto-indent-mode
        color-theme-solarized
        crontab-mode
        direx
        dockerfile-mode
        eclim
        el-get
        emmet-mode
        fill-column-indicator
        flycheck
        flycheck-color-mode-line
        flycheck-java
        flycheck-pos-tip ;; show flcheck messages in a tooltip
        flyspell-lazy
        ;; flx  ;; good fuzzy matching like sublime
        git-modes
        helm
        helm-etags-plus
        helm-ls-git
        helm-hoogle
        helm-git-grep
        jedi
        jedi-direx
        ; jss
        json-mode  ; has really nice beautifier
        ;; key-chord
        lalopmak-vimp
        less-css-mode
        longlines
        magit
        magit-gh-pulls
        markdown-mode
        multiple-cursors
        nix-mode
        paredit
        pbcopy
        php-mode
        projectile
        ;; plantuml-mode
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
