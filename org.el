
;; ORG MODE
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(setq org-directory "~/Dropbox/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")
(setq org-default-notes-file "~/Dropbox/org/general.org")

; Keywords
(setq org-todo-keywords '((sequence "TODO" "NEXT" "|" "DONE")))

; Clocking
(global-set-key (kbd "S-<f13>") 'org-clock-out)
(global-set-key (kbd "<f13>") 'org-clock-in-last)

; Persist clock history
(org-clock-persistence-insinuate)
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)

(global-set-key (kbd "<f12>") 'org-agenda-list)
(global-set-key (kbd "<f11>") 'org-capture)
(setq org-startup-indented 1)
(setq org-use-speed-commands t)
(add-hook 'org-mode-hook (lambda ()
                           (variable-pitch-mode t)
                           ()))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-restore-windows-after-quit nil)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

; Targets include this file and any file contributing to the agenda -
; up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; hide the slashes around emphasised words
(setq org-hide-emphasis-markers t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(setq org-capture-templates
      '(
        ("t" "Todo" entry
         (file+headline "~/Dropbox/org/general.org" "Tasks")
         "* TODO %?\n %T")
        ("a" "Question" entry
         (file+headline "~/Dropbox/org/general.org" "Questions")
         "*  %?\n %i\n  %a %U")
        ("r" "Activity" entry
         (file+datetree "~/Dropbox/org/journal.org" "Activities")
         "*  %?\n  %T")        
        ("j" "Thoughts" entry
         (file+datetree "~/Dropbox/org/thoughts.org" "Thoughts")
         "*  %?\n  %T")        
        )
      )

;;;;; Columns
(setq org-global-properties )
(setq org-columns-default-format " %25ITEM %TODO %17Effort(Estimated Effort){:} %CLOCKSUM")

;; org-babel

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (python . t)
   (sql . t)
   (ditaa . t)
   ))

(setq org-ditaa-jar-path "~/bin/ditaa.jar")

;; ORG-EXPORT

(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
  '("article"
"\\documentclass[11pt,a4paper]{article}
\\usepackage{fontspec}
\\usepackage{graphicx} 
\\usepackage{hyperref}
\\setromanfont{Palatino}
\\setmonofont{Menlo}
\\usepackage{geometry}
\\tracinglostchars=2
\\geometry{a4paper, textwidth=5.5in, textheight=10in,
            marginparsep=7pt, marginparwidth=.6in}
\\pagestyle{empty}
\\title{}    
      [NO-DEFAULT-PACKAGES]
      [NO-PACKAGES]"
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
     ("\\paragraph{%s}" . "\\paragraph*{%s}")
     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-latex-to-pdf-process 
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes
