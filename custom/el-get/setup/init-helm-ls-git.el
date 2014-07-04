(require 'helm-ls-git)
(defun file-name-at-point ()
       (cond ((region-active-p)
              (buffer-substring-no-properties region-beginning region-end))
             ("")))


(defun helm-ls-git-ls ()
  (interactive)
  (helm :sources '(helm-source-ls-git-status
                   helm-source-ls-git)
        ;; When `helm-ls-git-ls' is called from lisp
        ;; `default-directory' is normally let-bounded,
        ;; to some other value;
        ;; we now set this new let-bounded value local
        ;; to `helm-default-directory'.
        :input (file-name-at-point)
        :default-directory default-directory
        :buffer "*helm lsgit*"))
