(require 'helm-gtags)

;;;###autoload
(defun tw/helm-gtags-dwim ()
  "My version of helm-gtags-dwim, without the 'find tag from here
bit which is in the default version"
  (interactive)
  (let ((line (helm-current-line-contents)))
    (if (string-match helm-gtags--include-regexp line)
        (let ((helm-gtags-use-input-at-cursor t))
          (helm-gtags-find-files (match-string-no-properties 1 line)))
        (call-interactively 'helm-gtags-find-tag))))

(defun tw/gtags-search () 
  (interactive)
    (helm :sources (helm-build-async-source "test2"
                     :requires-pattern 2
                     :candidates-process
                     (lambda ()
                       (start-process "global" nil "global" "-x" "-i" "--path-style" "through" (concat ".*" (s-join ".*" (split-string helm-pattern " " )) ".*"))))
          :buffer "*helm async source*"
          :action (helm-make-actions "Go to file" nil))
    )

;;;###autoload
(defvar tw/helm-source-gtags
  '((name . "Gtags")
    (volatile)
    ;; (delayed)
    ;; (multiline)
    (requires-pattern . 2)
    (candidates-process . tw/gtags-search)
    ;; (action-transformer . helm-spotify-actions-for-track)
    ))

;;;###autoload
(defun tw/helm-gtags ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(tw/helm-source-gtags)
:buffer "*helm-gtags*"))