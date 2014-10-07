;; HOOKS
(add-hook 'python-mode-hook
      (lambda ()
        (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))))

(add-hook 'python-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(define-key python-mode-map (kbd "C-c d") 'jedi:show-doc)
(setq python-indent-offset 4)

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
