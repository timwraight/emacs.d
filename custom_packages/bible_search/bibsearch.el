(defvar helm-source-bible-search
  (helm-build-async-source "Bible Search"
    :candidates-process
    (lambda ()
      (start-process "bib_search" nil "bib_search"
                     helm-pattern))))

(defun helm-bible-search ()
  (interactive)
  (helm :sources 'helm-source-bible-search
        :buffer "*Helm Bible Search*"))
