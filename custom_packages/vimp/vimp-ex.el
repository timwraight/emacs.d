;;; vimp-ex.el --- Ex-mode

;; Author: Frank Fischer <frank fischer at mathematik.tu-chemnitz.de>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.2.8

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Ex is implemented as an extensible minilanguage, whose grammar
;; is stored in `vimp-ex-grammar'.  Ex commands are defined with
;; `vimp-ex-define-cmd', which creates a binding from a string
;; to an interactive function.  It is also possible to define key
;; sequences which execute a command immediately when entered:
;; such shortcuts go in `vimp-ex-map'.
;;
;; To provide buffer and filename completion, as well as interactive
;; feedback, Ex defines the concept of an argument handler, specified
;; with `vimp-ex-define-argument-type'.  In the case of the
;; substitution command (":s/foo/bar"), the handler incrementally
;; highlights matches in the buffer as the substitution is typed.

(require 'vimp-common)
(require 'vimp-states)

;;; Code:

(defconst vimp-ex-grammar
  '((expression
     (count command argument #'vimp-ex-call-command)
     ((\? range) command argument #'vimp-ex-call-command)
     (line #'vimp-goto-line)
     (sexp #'eval-expression))
    (count
     number)
    (command #'vimp-ex-parse-command)
    (binding
     "[~&*@<>=:]+\\|[[:alpha:]-]+\\|!")
    (emacs-binding
     "[[:alpha:]-][[:alnum:][:punct:]-]+")
    (bang
     (\? (! space) "!" #'$1))
    (argument
     ((\? space) (\? "\\(?:.\\|\n\\)+") #'$2))
    (range
     ("%" #'(vimp-ex-full-range))
     (line ";" line #'(let ((tmp1 $1))
                        (save-excursion
                          (goto-line tmp1)
                          (vimp-ex-range tmp1 $3))))
     (line "," line #'(vimp-ex-range $1 $3))
     (line #'(vimp-ex-range $1 nil))
     ("`" "[-a-zA-Z_<>']" ",`" "[-a-zA-Z_<>']"
      #'(vimp-ex-char-marker-range $2 $4)))
    (line
     (base (\? offset) search (\? offset)
           #'(let ((tmp (vimp-ex-line $1 $2)))
               (save-excursion
                 (goto-line tmp)
                 (vimp-ex-line $3 $4))))
     ((\? base) offset search (\? offset)
      #'(let ((tmp (vimp-ex-line $1 $2)))
          (save-excursion
            (goto-line tmp)
            (vimp-ex-line $3 $4))))
     (base (\? offset) #'vimp-ex-line)
     ((\? base) offset #'vimp-ex-line))
    (base
     number
     marker
     search
     ("\\^" #'(vimp-ex-first-line))
     ("\\$" #'(vimp-ex-last-line))
     ("\\." #'(vimp-ex-current-line)))
    (offset
     (+ signed-number #'+))
    (marker
     ("'" "[-a-zA-Z_<>']" #'(vimp-ex-marker $2)))
    (search
     forward
     backward
     next
     prev
     subst)
    (forward
     ("/" "\\(?:[\\].\\|[^/,; ]\\)+" (! "/")
      #'(vimp-ex-re-fwd $2))
     ("/" "\\(?:[\\].\\|[^/]\\)+" "/"
      #'(vimp-ex-re-fwd $2)))
    (backward
     ("\\?" "\\(?:[\\].\\|[^?,; ]\\)+" (! "\\?")
      #'(vimp-ex-re-bwd $2))
     ("\\?" "\\(?:[\\].\\|[^?]\\)+" "\\?"
      #'(vimp-ex-re-bwd $2)))
    (next
     "\\\\/" #'(vimp-ex-prev-search))
    (prev
     "\\\\\\?" #'(vimp-ex-prev-search))
    (subst
     "\\\\&" #'(vimp-ex-prev-search))
    (signed-number
     (sign (\? number) #'vimp-ex-signed-number))
    (sign
     "\\+\\|-" #'intern)
    (number
     "[0-9]+" #'string-to-number)
    (space
     "[ ]+")
    (sexp
     "(.*)" #'(car-safe (read-from-string $1))))
  "Grammar for Ex.
An association list of syntactic symbols and their definitions.
The first entry is the start symbol.  A symbol's definition may
reference other symbols, but the grammar cannot contain
left recursion.  See `vimp-parser' for a detailed explanation
of the syntax.")

(defvar vimp-ex-echo-overlay nil
  "Overlay used for displaying info messages during ex.")

(defun vimp-ex-p ()
  "Whether Ex is currently active."
  (and vimp-ex-current-buffer t))

(vimp-define-command vimp-ex (&optional initial-input)
  "Enter an Ex command.
The ex command line is initialized with the value of
INITIAL-INPUT. If the command is called interactively the initial
input depends on the current state. If the current state is
normal state and no count argument is given then the initial
input is empty. If a prefix count is given the initial input is
.,.+count. If the current state is visual state then the initial
input is the visual region '<,'> or `<,`>. If the value of the
global variable `vimp-ex-initial-input' is non-nil, its content
is appended to the line."
  :keep-visual t
  :repeat abort
  (interactive
   (list
    (let ((s (concat
              (cond
               ((and (vimp-visual-state-p)
                     vimp-ex-visual-char-range
                     (memq (vimp-visual-type) '(inclusive exclusive)))
                "`<,`>")
               ((vimp-visual-state-p)
                "'<,'>")
               (current-prefix-arg
                (let ((arg (prefix-numeric-value current-prefix-arg)))
                  (cond ((< arg 0) (setq arg (1+ arg)))
                        ((> arg 0) (setq arg (1- arg))))
                  (if (= arg 0) '(".")
                    (format ".,.%+d" arg)))))
              vimp-ex-initial-input)))
      (and (> (length s) 0) s))))
  (let ((vimp-ex-current-buffer (current-buffer))
        (vimp-ex-previous-command (unless initial-input
                                    (car-safe vimp-ex-history)))
        vimp-ex-argument-handler
        vimp-ex-info-string
        result)
    (minibuffer-with-setup-hook
        #'vimp-ex-setup
      (setq result
            (read-from-minibuffer
             ":"
             (or initial-input
                 (and vimp-ex-previous-command
                      (propertize vimp-ex-previous-command 'face 'shadow)))
             vimp-ex-completion-map
             nil
             'vimp-ex-history
             vimp-ex-previous-command
             t)))
    (vimp-ex-execute result)))

(defun vimp-ex-execute (result)
  "Execute RESULT as an ex command on `vimp-ex-current-buffer'."
  ;; empty input means repeating the previous command
  (when (zerop (length result))
    (setq result vimp-ex-previous-command))
  ;; parse data
  (vimp-ex-update nil nil nil result)
  ;; execute command
  (unless (zerop (length result))
    (if vimp-ex-expression
        (eval vimp-ex-expression)
      (user-error "Ex: syntax error"))))

(defun vimp-ex-delete-backward-char ()
  "Close the minibuffer if it is empty.
Otherwise behaves like `delete-backward-char'."
  (interactive)
  (call-interactively
   (if (zerop (length (minibuffer-contents)))
       #'abort-recursive-edit
     #'delete-backward-char)))

(defun vimp-ex-abort ()
  "Cancel ex state when another buffer is selected."
  (unless (minibufferp)
    (abort-recursive-edit)))

(defun vimp-ex-setup ()
  "Initialize Ex minibuffer.
This function registers several hooks that are used for the
interactive actions during ex state."
  (add-hook 'post-command-hook #'vimp-ex-abort)
  (add-hook 'after-change-functions #'vimp-ex-update nil t)
  (add-hook 'minibuffer-exit-hook #'vimp-ex-teardown)
  (when vimp-ex-previous-command
    (add-hook 'pre-command-hook #'vimp-ex-remove-default))
  (remove-hook 'minibuffer-setup-hook #'vimp-ex-setup)
  (with-no-warnings
    (make-variable-buffer-local 'completion-at-point-functions))
  (setq completion-at-point-functions
        '(vimp-ex-command-completion-at-point
          vimp-ex-argument-completion-at-point)))
(put 'vimp-ex-setup 'permanent-local-hook t)

(defun vimp-ex-teardown ()
  "Deinitialize Ex minibuffer.
Clean up everything set up by `vimp-ex-setup'."
  (remove-hook 'post-command-hook #'vimp-ex-abort)
  (remove-hook 'minibuffer-exit-hook #'vimp-ex-teardown)
  (remove-hook 'after-change-functions #'vimp-ex-update t)
  (when vimp-ex-argument-handler
    (let ((runner (vimp-ex-argument-handler-runner
                   vimp-ex-argument-handler)))
      (when runner
        (funcall runner 'stop)))))
(put 'vimp-ex-teardown 'permanent-local-hook t)

(defun vimp-ex-remove-default ()
  "Remove the default text shown in the ex minibuffer.
When ex starts, the previous command is shown enclosed in
parenthesis. This function removes this text when the first key
is pressed."
  (when (and (not (eq this-command 'exit-minibuffer))
             (/= (minibuffer-prompt-end) (point-max)))
    (if (eq this-command 'vimp-ex-delete-backward-char)
        (setq this-command 'ignore))
    (delete-minibuffer-contents))
  (remove-hook 'pre-command-hook #'vimp-ex-remove-default))
(put 'vimp-ex-remove-default 'permanent-local-hook t)

(defun vimp-ex-update (&optional beg end len string)
  "Update Ex variables when the minibuffer changes.
This function is usually called from `after-change-functions'
hook. If BEG is non-nil (which is the case when called from
`after-change-functions'), then an error description is shown
in case of incomplete or unknown commands."
  (let* ((prompt (minibuffer-prompt-end))
         (string (or string (buffer-substring prompt (point-max))))
         arg bang cmd count expr func handler range tree type)
    (cond
     ((and (eq this-command #'self-insert-command)
           (commandp (setq cmd (lookup-key vimp-ex-map string))))
      (setq vimp-ex-expression `(call-interactively #',cmd))
      (when (minibufferp)
        (exit-minibuffer)))
     (t
      (setq cmd nil)
      ;; store the buffer position of each character
      ;; as the `ex-index' text property
      (dotimes (i (length string))
        (add-text-properties
         i (1+ i) (list 'ex-index (+ i prompt)) string))
      (with-current-buffer vimp-ex-current-buffer
        (setq tree (vimp-ex-parse string t)
              expr (vimp-ex-parse string))
        (when (eq (car-safe expr) 'vimp-ex-call-command)
          (setq count (eval (nth 1 expr))
                cmd (eval (nth 2 expr))
                arg (eval (nth 3 expr))
                range (cond
                       ((vimp-range-p count)
                        count)
                       ((numberp count)
                        (vimp-ex-range count count)))
                bang (and (save-match-data (string-match ".!$" cmd)) t))))
      (setq vimp-ex-tree tree
            vimp-ex-expression expr
            vimp-ex-range range
            vimp-ex-cmd cmd
            vimp-ex-bang bang
            vimp-ex-argument arg)
      ;; test the current command
      (when (and cmd (minibufferp))
        (setq func (vimp-ex-completed-binding cmd t))
        (cond
         ;; update argument-handler
         (func
          (when (setq type (vimp-get-command-property
                            func :ex-arg))
            (setq handler (cdr-safe
                           (assoc type
                                  vimp-ex-argument-types))))
          (unless (eq handler vimp-ex-argument-handler)
            (let ((runner (and vimp-ex-argument-handler
                               (vimp-ex-argument-handler-runner
                                vimp-ex-argument-handler))))
              (when runner (funcall runner 'stop)))
            (setq vimp-ex-argument-handler handler)
            (let ((runner (and vimp-ex-argument-handler
                               (vimp-ex-argument-handler-runner
                                vimp-ex-argument-handler))))
              (when runner (funcall runner 'start vimp-ex-argument))))
          (let ((runner (and vimp-ex-argument-handler
                             (vimp-ex-argument-handler-runner
                              vimp-ex-argument-handler))))
            (when runner (funcall runner 'update vimp-ex-argument))))
         (beg
          ;; show error message only when called from `after-change-functions'
          (let ((n (length (all-completions cmd (vimp-ex-completion-table)))))
            (cond
             ((> n 1) (vimp-ex-echo "Incomplete command"))
             ((= n 0) (vimp-ex-echo "Unknown command")))))))))))
(put 'vimp-ex-update 'permanent-local-hook t)

(defun vimp-ex-echo (string &rest args)
  "Display a message after the current Ex command."
  (with-selected-window (minibuffer-window)
    (with-current-buffer (window-buffer (minibuffer-window))
      (unless (or vimp-no-display
                  (zerop (length string)))
        (let ((string (format " [%s]" (apply #'format string args)))
              (ov (or vimp-ex-echo-overlay
                      (setq vimp-ex-echo-overlay (make-overlay (point-min) (point-max) nil t t))))
              after-change-functions before-change-functions)
          (put-text-property 0 (length string) 'face 'vimp-ex-info string)
          ;; The following 'trick' causes point to be shown before the
          ;; message instead behind. It is shamelessly stolen from the
          ;; implementation of `minibuffer-message`.
          (put-text-property 0 1 'cursor t string)
          (move-overlay ov (point-max) (point-max))
          (overlay-put ov 'after-string string)
          (add-hook 'pre-command-hook #'vimp--ex-remove-echo-overlay nil t))))))

(defun vimp--ex-remove-echo-overlay ()
  "Remove echo overlay from ex minibuffer."
  (when vimp-ex-echo-overlay
    (delete-overlay vimp-ex-echo-overlay)
    (setq vimp-ex-echo-overlay nil))
  (remove-hook 'pre-command-hook 'vimp--ex-remove-echo-overlay t))

(defun vimp-ex-completion ()
  "Completes the current ex command or argument."
  (interactive)
  (let (after-change-functions)
    (vimp-ex-update)
    (completion-at-point)
    (remove-text-properties (minibuffer-prompt-end) (point-max) '(face nil vimp))))

(defun vimp-ex-command-completion-at-point ()
  (let ((context (vimp-ex-syntactic-context (1- (point)))))
    (when (memq 'command context)
      (let ((beg (or (get-text-property 0 'ex-index vimp-ex-cmd)
                     (point)))
            (end (1+ (or (get-text-property (1- (length vimp-ex-cmd))
                                            'ex-index
                                            vimp-ex-cmd)
                         (1- (point))))))
        (list beg end (vimp-ex-completion-table))))))

(defun vimp-ex-completion-table ()
  (cond
   ((eq vimp-ex-complete-emacs-commands nil)
    #'vimp-ex-command-collection)
   ((eq vimp-ex-complete-emacs-commands 'in-turn)
    (completion-table-in-turn
     #'vimp-ex-command-collection
     #'(lambda (str pred flag)
         (completion-table-with-predicate
          obarray #'commandp t str pred flag))))
   (t
    #'(lambda (str pred flag)
        (vimp-completion-table-concat
         #'vimp-ex-command-collection
         #'(lambda (str pred flag)
             (completion-table-with-predicate
              obarray #'commandp t str pred flag))
         str pred flag)))))

(defun vimp-completion-table-concat (table1 table2 string pred flag)
  (cond
   ((eq flag nil)
    (let ((result1 (try-completion string table1 pred))
          (result2 (try-completion string table2 pred)))
      (cond
       ((null result1) result2)
       ((null result2) result1)
       ((and (eq result1 t) (eq result2 t)) t)
       (t result1))))
   ((eq flag t)
    (delete-dups
     (append (all-completions string table1 pred)
             (all-completions string table2 pred))))
   ((eq flag 'lambda)
    (and (or (eq t (test-completion string table1 pred))
             (eq t (test-completion string table2 pred)))
         t))
   ((eq (car-safe flag) 'boundaries)
    (or (completion-boundaries string table1 pred (cdr flag))
        (completion-boundaries string table2 pred (cdr flag))))
   ((eq flag 'metadata)
    '(metadata (display-sort-function . vimp-ex-sort-completions)))))

(defun vimp-ex-sort-completions (completions)
  (sort completions
        #'(lambda (str1 str2)
            (let ((p1 (eq 'vimp-ex-commands (get-text-property 0 'face str1)))
                  (p2 (eq 'vimp-ex-commands (get-text-property 0 'face str2))))
              (if (equal p1 p2)
                  (string< str1 str2)
                p1)))))

(defun vimp-ex-command-collection (cmd predicate flag)
  "Called to complete a command."
  (let (commands)
    ;; append ! to all commands that may take a bang argument
    (dolist (cmd (mapcar #'car vimp-ex-commands))
      (push cmd commands)
      (if (vimp-ex-command-force-p cmd)
          (push (concat cmd "!") commands)))
    (when (eq vimp-ex-complete-emacs-commands t)
      (setq commands
            (mapcar #'(lambda (str) (propertize str 'face 'vimp-ex-commands))
                    commands)))
    (cond
     ((eq flag nil) (try-completion cmd commands predicate))
     ((eq flag t) (all-completions cmd commands predicate))
     ((eq flag 'lambda) (test-completion cmd commands))
     ((eq (car-safe flag) 'boundaries)
      `(boundaries 0 . ,(length (cdr flag)))))))

(defun vimp-ex-argument-completion-at-point ()
  (let ((context (vimp-ex-syntactic-context (1- (point)))))
    (when (memq 'argument context)
      (let* ((beg (or (and vimp-ex-argument
                           (get-text-property 0 'ex-index vimp-ex-argument))
                      (point)))
             (end (1+ (or (and vimp-ex-argument
                               (get-text-property (1- (length vimp-ex-argument))
                                                  'ex-index
                                                  vimp-ex-argument))
                          (1- (point)))))
             (binding (vimp-ex-completed-binding vimp-ex-cmd))
             (arg-type (vimp-get-command-property binding :ex-arg))
             (arg-handler (assoc arg-type vimp-ex-argument-types))
             (completer (and arg-handler
                             (vimp-ex-argument-handler-completer
                              (cdr arg-handler)))))
        (when completer
          (if (eq (car completer) 'collection)
              (list beg end (cdr completer))
            (save-restriction
              (narrow-to-region beg (point-max))
              (funcall (cdr completer)))))))))

(defun vimp-ex-define-cmd (cmd function)
  "Binds the function FUNCTION to the command CMD."
  (save-match-data
    (if (string-match "^[^][]*\\(\\[\\(.*\\)\\]\\)[^][]*$" cmd)
        (let ((abbrev (replace-match "" nil t cmd 1))
              (full (replace-match "\\2" nil nil cmd 1)))
          (vimp-add-to-alist 'vimp-ex-commands full function)
          (vimp-add-to-alist 'vimp-ex-commands abbrev full))
      (vimp-add-to-alist 'vimp-ex-commands cmd function))))

(defun vimp-ex-make-argument-handler (runner completer)
  (list runner completer))

(defun vimp-ex-argument-handler-runner (arg-handler)
  (car arg-handler))

(defun vimp-ex-argument-handler-completer (arg-handler)
  (cadr arg-handler))

(defmacro vimp-ex-define-argument-type (arg-type doc &rest body)
  "Defines a new handler for argument-type ARG-TYPE.
DOC is the documentation string. It is followed by a list of
keywords and function:

:collection COLLECTION

  A collection for completion as required by `all-completions'.

:completion-at-point FUNC

  Function to be called to initialize a potential
  completion. FUNC must match the requirements as described for
  the variable `completion-at-point-functions'. When FUNC is
  called the minibuffer content is narrowed to exactly match the
  argument.

:runner FUNC

  Function to be called when the type of the current argument
  changes or when the content of this argument changes. This
  function should take one obligatory argument FLAG followed by
  an optional argument ARG. FLAG is one of three symbol 'start,
  'stop or 'update. When the argument type is recognized for the
  first time and this handler is started the FLAG is 'start. If
  the argument type changes to something else or ex state
  finished the handler FLAG is 'stop. If the content of the
  argument has changed FLAG is 'update. If FLAG is either 'start
  or 'update then ARG is the current value of this argument. If
  FLAG is 'stop then arg is nil."
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp function-form]])))
  (unless (stringp doc) (push doc body))
  (let (runner completer)
    (while (keywordp (car-safe body))
      (let ((key (pop body))
            (func (pop body)))
        (cond
         ((eq key :runner)
          (setq runner func))
         ((eq key :collection)
          (setq completer (cons 'collection func)))
         ((eq key :completion-at-point)
          (setq completer (cons 'completion-at-point func))))))
    `(eval-and-compile
       (vimp-add-to-alist
        'vimp-ex-argument-types
        ',arg-type
        '(,runner ,completer)))))

(vimp-ex-define-argument-type file
  "Handles a file argument."
  :collection read-file-name-internal)

(vimp-ex-define-argument-type buffer
  "Called to complete a buffer name argument."
  :collection internal-complete-buffer)

(declare-function shell-completion-vars "shell" ())

(defun vimp-ex-init-shell-argument-completion (flag &optional arg)
  "Prepares the current minibuffer for completion of shell commands.
This function must be called from the :runner function of some
argument handler that requires shell completion."
  (when (and (eq flag 'start)
             (not vimp-ex-shell-argument-initialized)
             (require 'shell nil t)
             (require 'comint nil t))
    (set (make-local-variable 'vimp-ex-shell-argument-initialized) t)
    (cond
     ;; Emacs 24
     ((fboundp 'comint-completion-at-point)
      (shell-completion-vars))
     (t
      (set (make-local-variable 'minibuffer-default-add-function)
           'minibuffer-default-add-shell-commands)))
    (setq completion-at-point-functions
          '(vimp-ex-command-completion-at-point
            vimp-ex-argument-completion-at-point))))

;; because this variable is used only for Emacs 23 shell completion,
;; we put it here instead of "vimp-vars.el"
(defvar vimp-ex-shell-argument-range nil
  "Internal helper variable for Emacs 23 shell completion.")

(defun vimp-ex-shell-command-completion-at-point ()
  "Completion at point function for shell commands."
  (cond
   ;; Emacs 24
   ((fboundp 'comint-completion-at-point)
    (comint-completion-at-point))
   ;; Emacs 23
   ((fboundp 'minibuffer-complete-shell-command)
    (set (make-local-variable 'vimp-ex-shell-argument-range)
         (list (point-min) (point-max)))
    #'(lambda ()
        ;; We narrow the buffer to the argument so
        ;; `minibuffer-complete-shell-command' will correctly detect
        ;; the beginning of the argument.  When narrowing the buffer
        ;; to the argument the leading text in the minibuffer will be
        ;; hidden. Therefore we add a dummy overlay which shows that
        ;; text during narrowing.
        (let* ((beg (car vimp-ex-shell-argument-range))
               (end (cdr vimp-ex-shell-argument-range))
               (prev-text (buffer-substring
                           (point-min)
                           (car vimp-ex-shell-argument-range)))
               (ov (make-overlay beg beg)))
          (overlay-put ov 'before-string prev-text)
          (save-restriction
            (apply #'narrow-to-region vimp-ex-shell-argument-range)
            (minibuffer-complete-shell-command))
          (delete-overlay ov))))))

(vimp-ex-define-argument-type shell
  "Shell argument type, supports completion."
  :completion-at-point vimp-ex-shell-command-completion-at-point
  :runner vimp-ex-init-shell-argument-completion)

(defun vimp-ex-file-or-shell-command-completion-at-point ()
  (if (and (< (point-min) (point-max))
           (= (char-after (point-min)) ?!))
      (save-restriction
        (narrow-to-region (1+ (point-min)) (point-max))
        (vimp-ex-shell-command-completion-at-point))
    (list (point-min) (point-max) #'read-file-name-internal)))

(vimp-ex-define-argument-type file-or-shell
  "File or shell argument type.
If the current argument starts with a ! the rest of the argument
is considered a shell command, otherwise a file-name. Completion
works accordingly."
  :completion-at-point vimp-ex-file-or-shell-command-completion-at-point
  :runner vimp-ex-init-shell-argument-completion)

(defun vimp-ex-binding (command &optional noerror)
  "Returns the final binding of COMMAND."
  (save-match-data
    (let ((binding command))
      (when binding
        (string-match "^\\(.+?\\)\\!?$" binding)
        (setq binding (match-string 1 binding))
        (while (progn
                 (setq binding (cdr (assoc binding vimp-ex-commands)))
                 (stringp binding)))
        (unless binding
          (setq binding (intern command)))
        (if (commandp binding)
            binding
          (unless noerror
            (user-error "Unknown command: `%s'" command)))))))

(defun vimp-ex-completed-binding (command &optional noerror)
  "Returns the final binding of the completion of COMMAND."
  (let ((completion (try-completion command vimp-ex-commands)))
    (vimp-ex-binding (if (eq completion t) command
                       (or completion command))
                     noerror)))

;;; TODO: extensions likes :p :~ <cfile> ...
(defun vimp-ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME.
Replaces % by the current file-name,
Replaces # by the alternate file-name in FILE-NAME."
  (let ((current-fname (buffer-file-name))
        (alternate-fname (and (other-buffer)
                              (buffer-file-name (other-buffer)))))
    (when current-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(%\\)"
                                      current-fname file-name
                                      t t 2)))
    (when alternate-fname
      (setq file-name
            (replace-regexp-in-string "\\(^\\|[^\\\\]\\)\\(#\\)"
                                      alternate-fname file-name
                                      t t 2)))
    (setq file-name
          (replace-regexp-in-string "\\\\\\([#%]\\)"
                                    "\\1" file-name t)))
  file-name)

(defun vimp-ex-file-arg ()
  "Returns the current Ex argument as a file name.
This function interprets special file names like # and %."
  (unless (or (null vimp-ex-argument)
              (zerop (length vimp-ex-argument)))
    (vimp-ex-replace-special-filenames vimp-ex-argument)))

(defun vimp-ex-repeat (count)
  "Repeats the last ex command."
  (interactive "P")
  (when count
    (goto-char (point-min))
    (forward-line (1- count)))
  (let ((vimp-ex-current-buffer (current-buffer))
        (hist vimp-ex-history))
    (while hist
      (let ((vimp-ex-last-cmd (pop hist)))
        (when vimp-ex-last-cmd
          (vimp-ex-update nil nil nil vimp-ex-last-cmd)
          (let ((binding (vimp-ex-binding vimp-ex-cmd)))
            (unless (eq binding #'vimp-ex-repeat)
              (setq hist nil)
              (if vimp-ex-expression
                  (eval vimp-ex-expression)
                (user-error "Ex: syntax error")))))))))

(defun vimp-ex-call-command (range command argument)
  "Execute the given command COMMAND."
  (let* ((count (when (numberp range) range))
         (range (when (vimp-range-p range) range))
         (bang (and (save-match-data (string-match ".!$" command)) t))
         (vimp-ex-point (point))
         (vimp-ex-range
          (or range (and count (vimp-ex-range count count))))
         (vimp-ex-command (vimp-ex-completed-binding command))
         (vimp-ex-bang (and bang t))
         (vimp-ex-argument (copy-sequence argument))
         (vimp-this-type (vimp-type vimp-ex-range))
         (current-prefix-arg count)
         (prefix-arg current-prefix-arg))
    (when (stringp vimp-ex-argument)
      (set-text-properties
       0 (length vimp-ex-argument) nil vimp-ex-argument))
    (let ((buf (current-buffer)))
      (unwind-protect
          (if (not vimp-ex-range)
              (call-interactively vimp-ex-command)
            ;; set visual selection to match the region if an explicit
            ;; range has been specified
            (let ((ex-range (vimp-copy-range vimp-ex-range))
                  beg end)
              (vimp-expand-range ex-range)
              (setq beg (vimp-range-beginning ex-range)
                    end (vimp-range-end ex-range))
              (vimp-sort beg end)
              (set-mark end)
              (goto-char beg)
              (activate-mark)
              (call-interactively vimp-ex-command)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (deactivate-mark)))))))

(defun vimp-ex-line (base &optional offset)
  "Return the line number of BASE plus OFFSET."
  (+ (or base (line-number-at-pos))
     (or offset 0)))

(defun vimp-ex-first-line ()
  "Return the line number of the first line."
  (line-number-at-pos (point-min)))

(defun vimp-ex-current-line ()
  "Return the line number of the current line."
  (line-number-at-pos (point)))

(defun vimp-ex-last-line ()
  "Return the line number of the last line."
  (save-excursion
    (goto-char (point-max))
    (when (bolp)
      (forward-line -1))
    (line-number-at-pos)))

(defun vimp-ex-range (beg-line &optional end-line)
  "Returns the first and last position of the current range."
  (vimp-range
   (vimp-line-position beg-line)
   (vimp-line-position (or end-line beg-line) -1)
   'line
   :expanded t))

(defun vimp-ex-full-range ()
  "Return a range encompassing the whole buffer."
  (vimp-range (point-min) (point-max) 'line))

(defun vimp-ex-marker (marker)
  "Return MARKER's line number in the current buffer.
Signal an error if MARKER is in a different buffer."
  (when (stringp marker)
    (setq marker (aref marker 0)))
  (setq marker (vimp-get-marker marker))
  (if (numberp marker)
      (line-number-at-pos marker)
    (user-error "Ex does not support markers in other files")))

(defun vimp-ex-char-marker-range (beg end)
  (when (stringp beg) (setq beg (aref beg 0)))
  (when (stringp end) (setq end (aref end 0)))
  (setq beg (vimp-get-marker beg)
        end (vimp-get-marker end))
  (if (and (numberp beg) (numberp end))
      (vimp-expand-range
       (vimp-range beg end
                   (if (vimp-visual-state-p)
                       (vimp-visual-type)
                     'inclusive)))
    (user-error "Ex does not support markers in other files")))

(defun vimp-ex-re-fwd (pattern)
  "Search forward for PATTERN.
Returns the line number of the match."
  (condition-case err
      (save-match-data
        (save-excursion
          (set-text-properties 0 (length pattern) nil pattern)
          (vimp-move-end-of-line)
          (and (re-search-forward pattern nil t)
               (line-number-at-pos (1- (match-end 0))))))
    (invalid-regexp
     (vimp-ex-echo (cadr err))
     nil)))

(defun vimp-ex-re-bwd (pattern)
  "Search backward for PATTERN.
Returns the line number of the match."
  (condition-case err
      (save-match-data
        (save-excursion
          (set-text-properties 0 (length pattern) nil pattern)
          (vimp-move-beginning-of-line)
          (and (re-search-backward pattern nil t)
               (line-number-at-pos (match-beginning 0)))))
    (invalid-regexp
     (vimp-ex-echo (cadr err))
     nil)))

(defun vimp-ex-prev-search ()
  (error "Previous search not yet implemented"))

(defun vimp-ex-signed-number (sign &optional number)
  "Return a signed number like -3 and +1.
NUMBER defaults to 1."
  (funcall sign (or number 1)))

(defun vimp-ex-eval (string &optional start)
  "Evaluate STRING as an Ex command.
START is the start symbol, which defaults to `expression'."
  ;; disable the mark before executing, otherwise the visual region
  ;; may be used as operator range instead of the ex-range
  (let ((form (vimp-ex-parse string nil start))
        transient-mark-mode deactivate-mark)
    (eval form)))

(defun vimp-ex-parse (string &optional syntax start)
  "Parse STRING as an Ex expression and return an evaluation tree.
If SYNTAX is non-nil, return a syntax tree instead.
START is the start symbol, which defaults to `expression'."
  (let* ((start (or start (car-safe (car-safe vimp-ex-grammar))))
         (match (vimp-parser
                 string start vimp-ex-grammar t syntax)))
    (car-safe match)))

(defun vimp-ex-parse-command (string)
  "Parse STRING as an Ex binding."
  (let ((result (vimp-parser string 'binding vimp-ex-grammar))
        bang command)
    (when result
      (setq command (car-safe result)
            string (cdr-safe result))
      ;; check whether the command is followed by a slash and the
      ;; part before the slash is not a known ex binding
      ;; (maybe we should check for other characters, too? But only
      ;; the slash is used commonly in Emacs functions)
      (when (and (> (length string) 0)
                 (= (aref string 0) ?/)
                 (not (vimp-ex-binding command t)))
        ;; if this is the case, assume the slash and all following
        ;; symbol characters form an (Emacs-)command
        (setq result (vimp-parser (concat command string)
                                  'emacs-binding
                                  vimp-ex-grammar)
              command (car-safe result)
              string (cdr-safe result)))
      ;; parse a following "!" as bang only if
      ;; the command has the property :ex-bang t
      (when (vimp-ex-command-force-p command)
        (setq result (vimp-parser string 'bang vimp-ex-grammar)
              bang (or (car-safe result) "")
              string (cdr-safe result)
              command (concat command bang)))
      (cons command string))))

(defun vimp-ex-command-force-p (command)
  "Whether COMMAND accepts the bang argument."
  (let ((binding (vimp-ex-completed-binding command t)))
    (when binding
      (vimp-get-command-property binding :ex-bang))))

(defun vimp-flatten-syntax-tree (tree)
  "Find all paths from the root of TREE to its leaves.
TREE is a syntax tree, i.e., all its leave nodes are strings.
The `nth' element in the result is the syntactic context
for the corresponding string index (counted from zero)."
  (let* ((result nil)
         (traverse nil)
         (traverse
          #'(lambda (tree path)
              (if (stringp tree)
                  (dotimes (char (length tree))
                    (push path result))
                (let ((path (cons (car tree) path)))
                  (dolist (subtree (cdr tree))
                    (funcall traverse subtree path)))))))
    (funcall traverse tree nil)
    (nreverse result)))

(defun vimp-ex-syntactic-context (&optional pos)
  "Return the syntactical context of the character at POS.
POS defaults to the current position of point."
  (let* ((contexts (vimp-flatten-syntax-tree vimp-ex-tree))
         (length (length contexts))
         (pos (- (or pos (point)) (minibuffer-prompt-end))))
    (when (>= pos length)
      (setq pos (1- length)))
    (when (< pos 0)
      (setq pos 0))
    (when contexts
      (nth pos contexts))))

(defun vimp-parser (string symbol grammar &optional greedy syntax)
  "Parse STRING as a SYMBOL in GRAMMAR.
If GREEDY is non-nil, the whole of STRING must match.
If the parse succeeds, the return value is a cons cell
\(RESULT . TAIL), where RESULT is a parse tree and TAIL is
the remainder of STRING. Otherwise, the return value is nil.

GRAMMAR is an association list of symbols and their definitions.
A definition is either a list of production rules, which are
tried in succession, or a #'-quoted function, which is called
to parse the input.

A production rule can be one of the following:

    nil matches the empty string.
    A regular expression matches a substring.
    A symbol matches a production for that symbol.
    (X Y) matches X followed by Y.
    (\\? X) matches zero or one of X.
    (* X) matches zero or more of X.
    (+ X) matches one or more of X.
    (& X) matches X, but does not consume.
    (! X) matches anything but X, but does not consume.

Thus, a simple grammar may look like:

    ((plus \"\\\\+\")           ; plus <- \"+\"
     (minus \"-\")            ; minus <- \"-\"
     (operator plus minus)) ; operator <- plus / minus

All input-consuming rules have a value. A regular expression evaluates
to the text matched, while a list evaluates to a list of values.
The value of a list may be overridden with a semantic action, which is
specified with a #'-quoted expression at the end:

    (X Y #'foo)

The value of this rule is the result of calling foo with the values
of X and Y as arguments. Alternatively, the function call may be
specified explicitly:

    (X Y #'(foo $1 $2))

Here, $1 refers to X and $2 refers to Y. $0 refers to the whole list.
Dollar expressions can also be used directly:

    (X Y #'$1)

This matches X followed by Y, but ignores the value of Y;
the value of the list is the same as the value of X.

If the SYNTAX argument is non-nil, then all semantic actions
are ignored, and a syntax tree is constructed instead. The
syntax tree obeys the property that all the leave nodes are
parts of the input string. Thus, by traversing the syntax tree,
one can determine how each character was parsed.

The following symbols have reserved meanings within a grammar:
`\\?', `*', `+', `&', `!', `function', `alt', `seq' and nil."
  (let ((string (or string ""))
        func pair result rules tail)
    (cond
     ;; epsilon
     ((member symbol '("" nil))
      (setq pair (cons (if syntax "" nil) string)))
     ;; token
     ((stringp symbol)
      (save-match-data
        (when (or (eq (string-match symbol string) 0)
                  ;; ignore leading whitespace
                  (and (eq (string-match "^[ \f\t\n\r\v]+" string) 0)
                       (eq (match-end 0)
                           (string-match
                            symbol string (match-end 0)))))
          (setq result (match-string 0 string)
                tail (substring string (match-end 0))
                pair (cons result tail))
          (when (and syntax pair)
            (setq result (substring string 0
                                    (- (length string)
                                       (length tail))))
            (setcar pair result)))))
     ;; symbol
     ((symbolp symbol)
      (let ((context symbol))
        (setq rules (cdr-safe (assq symbol grammar)))
        (setq pair (vimp-parser string `(alt ,@rules)
                                grammar greedy syntax))
        (when (and syntax pair)
          (setq result (car pair))
          (if (and (listp result) (sequencep (car result)))
              (setq result `(,symbol ,@result))
            (setq result `(,symbol ,result)))
          (setcar pair result))))
     ;; function
     ((eq (car-safe symbol) 'function)
      (setq symbol (cadr symbol)
            pair (funcall symbol string))
      (when (and syntax pair)
        (setq tail (or (cdr pair) "")
              result (substring string 0
                                (- (length string)
                                   (length tail))))
        (setcar pair result)))
     ;; list
     ((listp symbol)
      (setq rules symbol
            symbol (car-safe rules))
      (if (memq symbol '(& ! \? * + alt seq))
          (setq rules (cdr rules))
        (setq symbol 'seq))
      (when (and (memq symbol '(+ alt seq))
                 (> (length rules) 1))
        (setq func (car (last rules)))
        (if (eq (car-safe func) 'function)
            (setq rules (delq func (copy-sequence rules))
                  func (cadr func))
          (setq func nil)))
      (cond
       ;; positive lookahead
       ((eq symbol '&)
        (when (vimp-parser string rules grammar greedy syntax)
          (setq pair (vimp-parser string nil grammar nil syntax))))
       ;; negative lookahead
       ((eq symbol '!)
        (unless (vimp-parser string rules grammar greedy syntax)
          (setq pair (vimp-parser string nil grammar nil syntax))))
       ;; zero or one
       ((eq symbol '\?)
        (setq rules (if (> (length rules) 1)
                        `(alt ,rules nil)
                      `(alt ,@rules nil))
              pair (vimp-parser string rules grammar greedy syntax)))
       ;; zero or more
       ((eq symbol '*)
        (setq rules `(alt (+ ,@rules) nil)
              pair (vimp-parser string rules grammar greedy syntax)))
       ;; one or more
       ((eq symbol '+)
        (let (current results)
          (catch 'done
            (while (setq current (vimp-parser
                                  string rules grammar nil syntax))
              (setq result (car-safe current)
                    tail (or (cdr-safe current) "")
                    results (append results (if syntax result
                                              (cdr-safe result))))
              ;; stop if stuck
              (if (equal string tail)
                  (throw 'done nil)
                (setq string tail))))
          (when results
            (setq func (or func 'list)
                  pair (cons results tail)))))
       ;; alternatives
       ((eq symbol 'alt)
        (catch 'done
          (dolist (rule rules)
            (when (setq pair (vimp-parser
                              string rule grammar greedy syntax))
              (throw 'done pair)))))
       ;; sequence
       (t
        (setq func (or func 'list))
        (let ((last (car-safe (last rules)))
              current results rule)
          (catch 'done
            (while rules
              (setq rule (pop rules)
                    current (vimp-parser string rule grammar
                                         (when greedy
                                           (null rules))
                                         syntax))
              (cond
               ((null current)
                (setq results nil)
                (throw 'done nil))
               (t
                (setq result (car-safe current)
                      tail (cdr-safe current))
                (unless (memq (car-safe rule) '(& !))
                  (if (and syntax
                           (or (null result)
                               (and (listp result)
                                    (listp rule)
                                    ;; splice in single-element
                                    ;; (\? ...) expressions
                                    (not (and (eq (car-safe rule) '\?)
                                              (eq (length rule) 2))))))
                      (setq results (append results result))
                    (setq results (append results (list result)))))
                (setq string (or tail ""))))))
          (when results
            (setq pair (cons results tail))))))
      ;; semantic action
      (when (and pair func (not syntax))
        (setq result (car pair))
        (let* ((dexp
                #'(lambda (obj)
                    (when (symbolp obj)
                      (let ((str (symbol-name obj)))
                        (save-match-data
                          (when (string-match "\\$\\([0-9]+\\)" str)
                            (string-to-number (match-string 1 str))))))))
               ;; traverse a tree for dollar expressions
               (dval nil)
               (dval
                #'(lambda (obj)
                    (if (listp obj)
                        (mapcar dval obj)
                      (let ((num (funcall dexp obj)))
                        (if num
                            (if (not (listp result))
                                result
                              (if (eq num 0)
                                  `(list ,@result)
                                (nth (1- num) result)))
                          obj))))))
          (cond
           ((null func)
            (setq result nil))
           ;; lambda function
           ((eq (car-safe func) 'lambda)
            (if (memq symbol '(+ seq))
                (setq result `(funcall ,func ,@result))
              (setq result `(funcall ,func ,result))))
           ;; string replacement
           ((or (stringp func) (stringp (car-safe func)))
            (let* ((symbol (or (car-safe (cdr-safe func))
                               (and (boundp 'context) context)
                               (car-safe (car-safe grammar))))
                   (string (if (stringp func) func (car-safe func))))
              (setq result (car-safe (vimp-parser string symbol grammar
                                                  greedy syntax)))))
           ;; dollar expression
           ((funcall dexp func)
            (setq result (funcall dval func)))
           ;; function call
           ((listp func)
            (setq result (funcall dval func)))
           ;; symbol
           (t
            (if (memq symbol '(+ seq))
                (setq result `(,func ,@result))
              (setq result `(,func ,result))))))
        (setcar pair result))))
    ;; weed out incomplete matches
    (when pair
      (if (not greedy) pair
        (if (null (cdr pair)) pair
          ;; ignore trailing whitespace
          (when (save-match-data (string-match "^[ \f\t\n\r\v]*$" (cdr pair)))
            (unless syntax (setcdr pair nil))
            pair))))))

(provide 'vimp-ex)

;;; vimp-ex.el ends here
