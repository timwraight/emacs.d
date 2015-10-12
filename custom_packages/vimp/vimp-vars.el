;;; vimp-vars.el --- Settings and variables

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.2.5

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

;;; Code:

(declare-function vimp-add-command-properties "vimp-common"
                  (command &rest properties))

;;; Hooks

(defvar vimp-after-load-hook nil
  "Functions to be run when loading of vimp is finished.
This hook can be used the execute some initialization routines
when vimp is completely loaded.")

;;; Initialization

(defvar vimp-pending-custom-initialize nil
  "A list of pending initializations for custom variables.
Each element is a triple (FUNC VAR VALUE). When vimp is
completely loaded then the functions (funcall FUNC VAR VALUE) is
called for each element. FUNC should be a function suitable for
the :initialize property of `defcustom'.")

(defun vimp-custom-initialize-pending-reset (var value)
  "Add a pending customization with `custom-initialize-reset'."
  (push (list 'custom-initialize-reset var value)
        vimp-pending-custom-initialize))

(defun vimp-run-pending-custom-initialize ()
  "Executes the pending initializations.
See `vimp-pending-custom-initialize'."
  (dolist (init vimp-pending-custom-initialize)
    (apply (car init) (cdr init)))
  (remove-hook 'vimp-after-load-hook 'vimp-run-pending-custom-initialize))
(add-hook 'vimp-after-load-hook 'vimp-run-pending-custom-initialize)

;;; Setters

(defun vimp-set-toggle-key (key)
  "Set `vimp-toggle-key' to KEY.
KEY must be readable by `read-kbd-macro'."
  (let ((old-key (read-kbd-macro
                  (if (boundp 'vimp-toggle-key)
                      vimp-toggle-key
                    "C-z")))
        (key (read-kbd-macro key)))
    (with-no-warnings
      (dolist (pair '((vimp-motion-state-map vimp-emacs-state)
                      (vimp-insert-state-map vimp-emacs-state)
                      (vimp-emacs-state-map vimp-exit-emacs-state)))
        (when (boundp (car pair))
          (let ((map (symbol-value (car pair)))
                (fun (cadr pair)))
            (when (keymapp map)
              (define-key map key fun)
              (define-key map old-key nil))))))))

(defun vimp-set-custom-state-maps (var pending-var key make newlist)
  "Changes the list of special keymaps.
VAR         is the variable containing the list of keymaps.
PENDING-VAR is the variable containing the list of the currently pending
            keymaps.
KEY         the special symbol to be stored in the keymaps.
MAKE        the creation function of the special keymaps.
NEWLIST     the list of new special keymaps."
  (set-default pending-var newlist)
  (when (default-boundp var)
    (dolist (map (default-value var))
      (when (and (boundp (car map))
                 (keymapp (default-value (car map))))
        (define-key (default-value (car map)) (vector key) nil))))
  (set-default var newlist)
  (vimp-update-pending-maps))

(defun vimp-update-pending-maps (&optional file)
  "Tries to set pending special keymaps.
This function should be called from an `after-load-functions'
hook."
  (let ((maps '((vimp-make-overriding-map . vimp-pending-overriding-maps)
                (vimp-make-intercept-map . vimp-pending-intercept-maps))))
    (while maps
      (let* ((map (pop maps))
             (make (car map))
             (pending-var (cdr map))
             (pending (symbol-value pending-var))
             newlist)
        (while pending
          (let* ((map (pop pending))
                 (kmap (and (boundp (car map))
                            (keymapp (symbol-value (car map)))
                            (symbol-value (car map))))
                 (state (cdr map)))
            (if kmap
                (funcall make kmap state)
              (push map newlist))))
        (set-default pending-var newlist)))))

(defun vimp-set-visual-newline-commands (var value)
  "Set the value of `vimp-visual-newline-commands'.
Setting this variable changes the properties of the appropriate
commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (cmd (default-value var))
        (vimp-set-command-property cmd :exclude-newline nil)))
    (set-default var value)
    (dolist (cmd (default-value var))
      (vimp-set-command-property cmd :exclude-newline t))))

(defun vimp-set-custom-motions (var values)
  "Sets the list of motion commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (motion (default-value var))
        (vimp-add-command-properties motion :keep-visual nil :repeat nil)))
    (set-default var values)
    (mapc #'vimp-declare-motion (default-value var))))

;;; Customization group

(defgroup vimp nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'vimp-)

(defcustom vimp-auto-indent t
  "Whether to auto-indent when entering Insert state."
  :type  'boolean
  :group 'vimp)
(make-variable-buffer-local 'vimp-auto-indent)

(defcustom vimp-shift-width 4
  "The offset used by \\<vimp-normal-state-map>\\[vimp-shift-right] \
and \\[vimp-shift-left]."
  :type 'integer
  :group 'vimp)
(make-variable-buffer-local 'vimp-shift-width)

(defcustom vimp-shift-round t
  "Whether \\<vimp-normal-state-map>\\[vimp-shift-right] \
and \\[vimp-shift-left] round to the nearest multiple \
of `vimp-shift-width'."
  :type 'boolean
  :group 'vimp)
(make-variable-buffer-local 'vimp-shift-round)

(defcustom vimp-default-cursor t
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'vimp)

(defcustom vimp-repeat-move-cursor t
  "Whether \"\\<vimp-normal-state-map>\\[vimp-repeat]\" \
moves the cursor."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-cross-lines nil
  "Whether motions may cross newlines."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-backspace-join-lines t
  "Whether backward delete in insert state may join lines."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-move-cursor-back t
  "Whether the cursor is moved backwards when exiting Insert state."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-move-beyond-eol nil
  "Whether the cursor is allowed to move past the last character of \
a line."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-repeat-find-to-skip-next t
  "Whether a repeat of t or T should skip an adjacent character."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-kbd-macro-suppress-motion-error nil
  "Whether left/right motions signal errors during keyboard-macro definition.
If this variable is set to non-nil, then the function
`vimp-forward-char' and `vimp-backward-char' do not signal
`end-of-line' or `beginning-of-line' errors when a keyboard macro
is being defined and/or it is being executed. This may be desired
because such an error would cause the macro definition/execution
being terminated."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Record" :value record)
                (const :tag "Replay" :value replay)
                (const :tag "Both" :value t))
  :group 'vimp)

(defcustom vimp-track-eol t
  "If non-nil line moves after a call to `vimp-end-of-line' stay at eol.
This is analogous to `track-eol' but deals with the end-of-line
interpretation of vimp."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-mode-line-format 'before
  "The position of the mode line tag.
Either a symbol or a cons-cell. If it is a symbol it should be
one of 'before, 'after or 'nil. 'before mean the the tag is
placed before the mode-list, 'after means it is placed after the
mode-list, and 'nil means no mode line tag. If it is a cons cell
it should have the form (WHERE . WHICH) where WHERE is either
'before or 'after and WHICH is a symbol in
`mode-line-format'. The tag is then placed right before or after
that symbol."
  :type '(radio :value 'before
                (const before)
                (const after)
                (cons :tag "Next to symbol"
                      (choice :value after
                              (const before)
                              (const after))
                      symbol))
  :group 'vimp)

(defcustom vimp-mouse-word 'vimp-word
  "The thing-at-point symbol for double click selection.
The double-click starts visual state in a special word selection
mode. This symbol is used to determine the words to be
selected. Possible values are 'vimp-word or
'vimp-WORD."
  :type 'symbol
  :group 'vimp)

(defcustom vimp-bigword "^ \t\r\n"
  "The characters to be considered as a big word.
This should be a regexp set without the enclosing []."
  :type 'string
  :group 'vimp)
(make-variable-buffer-local 'vimp-bigword)

(defcustom vimp-want-fine-undo 'fine
  "Whether actions like \"cw\" are undone in several steps.
There are three possible choices. \"No\" means all chances made
during insert state including a possible delete after a change
operation are collected in a single undo step. If \"Fine\" is
selected, insertion commands create several undo steps as Emacs
would do and the delete after a change operation is merged with
the first undo step of the insertion. \"Very fine\" is the same
but the delete operation is a separate undo step."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Fine" :value fine)
                (const :tag "Very fine" :value t))
  :group 'vimp)

(defcustom vimp-regexp-search t
  "Whether to use regular expressions for searching."
  :type  'boolean
  :group 'vimp)

(defcustom vimp-search-wrap t
  "Whether search wraps around."
  :type  'boolean
  :group 'vimp)

(defcustom vimp-flash-delay 2
  "Time in seconds to flash search matches."
  :type  'number
  :group 'vimp)

(defcustom vimp-fold-level 0
  "Default fold level."
  :type  'integer
  :group 'vimp)

(defcustom vimp-auto-balance-windows t
  "If non-nil creating/deleting a window causes a rebalance."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-split-window-below nil
  "If non-nil split windows are created below."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-vsplit-window-right nil
  "If non-nil vsplit windows are created to the right."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-esc-delay 0.01
  "Time in seconds to wait for another key after ESC."
  :type 'number
  :group 'vimp)

(defvar vimp-esc-mode nil
  "Non-nil if `vimp-esc-mode' is enabled.")

(defvar vimp-esc-map nil
  "Original ESC prefix map in `input-decode-map'.
Used by `vimp-esc-mode'.")

(defvar vimp-inhibit-esc nil
  "If non-nil, the \\e event will never be translated to 'escape.")

(defcustom vimp-intercept-esc 'always
  "Whether vimp should intercept the ESC key.
In terminal, a plain ESC key and a meta-key-sequence both
generate the same event. In order to distinguish both vimp
modifies `input-decode-map'. This is necessary in terminal but
not in X mode. However, the terminal ESC is equivalent to C-[, so
if you want to use C-[ instead of ESC in X, then Evil must
intercept the ESC event in X, too. This variable determines when
Evil should intercept the event."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "In terminal only" :value t)
                (const :tag "Always" :value always))
  :group 'vimp)

(defcustom vimp-show-paren-range 0
  "The minimal distance between point and a parenthesis
which causes the parenthesis to be highlighted."
  :type 'integer
  :group 'vimp)

(defcustom vimp-ex-hl-update-delay 0.02
  "Time in seconds of idle before updating search highlighting.
Setting this to a period shorter than that of keyboard's repeat
rate allows highlights to update while scrolling."
  :type 'number
  :group 'vimp)

(defcustom vimp-highlight-closing-paren-at-point-states
  '(not emacs insert replace)
  "The states in which the closing parenthesis at point should be highlighted.
All states listed here highlight the closing parenthesis at
point (which is Vim default behavior), all others highlight the
parenthesis before point (which is Emacs default behavior). If
this list contains the symbol 'not then its meaning is inverted,
i.e., all states listed here highlight the closing parenthesis
before point."
  :type '(repeat symbol)
  :group 'vimp)

(defcustom vimp-want-C-i-jump t
  "Whether \"C-i\" jumps forward like in Vim."
  :type 'boolean
  :group 'vimp
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'vimp-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key vimp-motion-state-map (kbd "C-i"))
                        'vimp-jump-forward))
               (define-key vimp-motion-state-map (kbd "C-i") nil))
              ((and value
                    (not (lookup-key vimp-motion-state-map (kbd "C-i"))))
               (define-key vimp-motion-state-map (kbd "C-i") 'vimp-jump-forward))))))

(defcustom vimp-want-C-u-scroll nil
  "Whether \"C-u\" scrolls like in Vim."
  :type 'boolean
  :group 'vimp
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'vimp-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key vimp-motion-state-map (kbd "C-u"))
                        'vimp-scroll-up))
               (define-key vimp-motion-state-map (kbd "C-u") nil))
              ((and value
                    (not (lookup-key vimp-motion-state-map (kbd "C-u"))))
               (define-key vimp-motion-state-map (kbd "C-u") 'vimp-scroll-up))))))

(defcustom vimp-want-C-w-delete t
  "Whether \"C-w\" deletes a word in Insert state."
  :type 'boolean
  :group 'vimp
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'vimp-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key vimp-motion-state-map (kbd "C-w"))
                        'vimp-delete-backward-word))
               (define-key vimp-motion-state-map (kbd "C-w") 'vimp-window-map))
              ((and value
                    (eq (lookup-key vimp-motion-state-map (kbd "C-u"))
                        'vimp-window-map))
               (define-key vimp-motion-state-map (kbd "C-u") 'vimp-delete-backward-word))))))

(defcustom vimp-want-C-w-in-emacs-state nil
  "Whether \"C-w\" prefixes windows commands in Emacs state."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-want-change-word-to-end t
  "Whether \"cw\" behaves like \"ce\"."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-want-Y-yank-to-eol nil
  "Whether \"Y\" yanks to the end of the line.
The default behavior is to yank the whole line."
  :group 'vimp
  :type 'boolean
  :initialize #'vimp-custom-initialize-pending-reset
  :set #'(lambda (sym value)
           (vimp-add-command-properties
            'vimp-yank-line
            :motion (if value 'vimp-end-of-line 'vimp-line))))

(defcustom vimp-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-complete-all-buffers t
  "Whether completion looks for matches in all buffers."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-complete-next-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless vimp-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (condition-case nil
            (if (eq last-command this-command)
                (dabbrev-expand nil)
              (dabbrev-expand (- (abs (or arg 1)))))
          (error (dabbrev-expand nil)))))
  "Completion function used by \
\\<vimp-insert-state-map>\\[vimp-complete-next]."
  :type 'function
  :group 'vimp)

(defcustom vimp-complete-previous-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless vimp-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (dabbrev-expand arg)))
  "Completion function used by \
\\<vimp-insert-state-map>\\[vimp-complete-previous]."
  :type 'function
  :group 'vimp)

(defcustom vimp-complete-next-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<vimp-insert-state-map>\\[vimp-complete-next]."
  :type 'function
  :group 'vimp)

(defcustom vimp-complete-previous-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<vimp-insert-state-map>\\[vimp-complete-previous]."
  :type 'function
  :group 'vimp)

(defcustom vimp-complete-next-line-func
  #'(lambda (arg)
      (let ((hippie-expand-try-functions-list
             '(try-expand-line
               try-expand-line-all-buffers)))
        (hippie-expand arg)))
  "Minibuffer completion function used by \
\\<vimp-insert-state-map>\\[vimp-complete-next-line]."
  :type 'function
  :group 'vimp)

(defcustom vimp-complete-previous-line-func
  vimp-complete-next-line-func
  "Minibuffer completion function used by \
\\<vimp-insert-state-map>\\[vimp-complete-previous-line]."
  :type 'function
  :group 'vimp)

(defcustom vimp-lookup-func #'woman
  "Lookup function used by \
\"\\<vimp-motion-state-map>\\[vimp-lookup]\"."
  :type 'function
  :group 'vimp)

(defcustom vimp-toggle-key "C-z"
  "The key used to change to and from Emacs state.
Must be readable by `read-kbd-macro'. For example: \"C-z\"."
  :type 'string
  :group 'vimp
  :set #'(lambda (sym value)
           (vimp-set-toggle-key value)
           (set-default sym value)))

(defcustom vimp-default-state 'normal
  "The default state.
This is the state a mode comes up in when it is not listed
in `vimp-emacs-state-modes', `vimp-insert-state-modes' or
`vimp-motion-state-modes'. The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and
`emacs'."
  :type  'symbol
  :group 'vimp)

(defcustom vimp-buffer-regexps
  '(("^ \\*load\\*" . nil))
  "Regular expression determining the initial state for a buffer.
Entries have the form (REGEXP . STATE), where REGEXP is a regular
expression matching the buffer's name and STATE is one of `normal',
`insert', `visual', `replace', `operator', `motion', `emacs' and nil.
If STATE is nil, Evil is disabled in the buffer."
  :type '(alist :key-type string :value-type symbol)
  :group 'vimp)

(defcustom vimp-emacs-state-modes
  '(archive-mode
    bbdb-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    ediff-meta-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    git-commit-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-cherry-mode
    magit-diff-mode
    magit-log-mode
    magit-log-select-mode
    magit-popup-mode
    magit-popup-sequence-mode
    magit-process-mode
    magit-reflog-mode
    magit-refs-mode
    magit-revision-mode
    magit-stash-mode
    magit-stashes-mode
    magit-status-mode
    ;; Obsolete as of Magit v2.1.0
    magit-mode
    magit-branch-manager-mode
    magit-commit-mode
    magit-key-mode
    magit-rebase-mode
    magit-wazzup-mode
    ;; end obsolete
    mh-folder-mode
    monky-mode
    mu4e-main-mode
    mu4e-headers-mode
    mu4e-view-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-select-label-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-hg-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in Emacs state."
  :type  '(repeat symbol)
  :group 'vimp)

(defcustom vimp-insert-state-modes
  '(comint-mode
    erc-mode
    eshell-mode
    geiser-repl-mode
    gud-mode
    inferior-apl-mode
    inferior-caml-mode
    inferior-emacs-lisp-mode
    inferior-j-mode
    inferior-python-mode
    inferior-scheme-mode
    inferior-sml-mode
    internal-ange-ftp-mode
    prolog-inferior-mode
    reb-mode
    shell-mode
    slime-repl-mode
    term-mode
    wdired-mode)
  "Modes that should come up in Insert state."
  :type  '(repeat symbol)
  :group 'vimp)

(defcustom vimp-motion-state-modes
  '(apropos-mode
    Buffer-menu-mode
    calendar-mode
    color-theme-mode
    command-history-mode
    compilation-mode
    dictionary-mode
    ert-results-mode
    help-mode
    Info-mode
    Man-mode
    speedbar-mode
    undo-tree-visualizer-mode
    view-mode
    woman-mode)
  "Modes that should come up in Motion state."
  :type  '(repeat symbol)
  :group 'vimp)

(defvar vimp-pending-overriding-maps nil
  "An alist of pending overriding maps.")

(defvar vimp-pending-intercept-maps nil
  "An alist of pending intercept maps.")

(defcustom vimp-overriding-maps
  '((Buffer-menu-mode-map . nil)
    (color-theme-mode-map . nil)
    (comint-mode-map . nil)
    (compilation-mode-map . nil)
    (grep-mode-map . nil)
    (dictionary-mode-map . nil)
    (ert-results-mode-map . motion)
    (Info-mode-map . motion)
    (speedbar-key-map . nil)
    (speedbar-file-key-map . nil)
    (speedbar-buffers-key-map . nil))
  "Keymaps that should override Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be overridden. If STATE is nil, all states are
overridden."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'vimp
  :set #'(lambda (var values)
           (vimp-set-custom-state-maps 'vimp-overriding-maps
                                       'vimp-pending-overriding-maps
                                       'override-state
                                       'vimp-make-overriding-map
                                       values))
  :initialize 'vimp-custom-initialize-pending-reset)

(add-hook 'after-load-functions #'vimp-update-pending-maps)

(defcustom vimp-intercept-maps
  '((edebug-mode-map . nil))
  "Keymaps that should intercept Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be intercepted. If STATE is nil, all states are
intercepted."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'vimp
  :set #'(lambda (var values)
           (vimp-set-custom-state-maps 'vimp-intercept-maps
                                       'vimp-pending-intercept-maps
                                       'intercept-state
                                       'vimp-make-intercept-map
                                       values))
  :initialize 'vimp-custom-initialize-pending-reset)

(defcustom vimp-motions
  '(back-to-indentation
    backward-char
    backward-list
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-up-list
    backward-word
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-visual-line
    c-beginning-of-defun
    c-end-of-defun
    diff-file-next
    diff-file-prev
    diff-hunk-next
    diff-hunk-prev
    down-list
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-visual-line
    exchange-point-and-mark
    forward-char
    forward-list
    forward-paragraph
    forward-sentence
    forward-sexp
    forward-word
    goto-last-change
    ibuffer-backward-line
    ibuffer-forward-line
    isearch-abort
    isearch-cancel
    isearch-complete
    isearch-del-char
    isearch-delete-char
    isearch-edit-string
    isearch-exit
    isearch-highlight-regexp
    isearch-occur
    isearch-other-control-char
    isearch-other-meta-char
    isearch-printing-char
    isearch-query-replace
    isearch-query-replace-regexp
    isearch-quote-char
    isearch-repeat-backward
    isearch-repeat-forward
    isearch-ring-advance
    isearch-ring-retreat
    isearch-toggle-case-fold
    isearch-toggle-input-method
    isearch-toggle-regexp
    isearch-toggle-specified-input-method
    isearch-toggle-word
    isearch-yank-char
    isearch-yank-kill
    isearch-yank-line
    isearch-yank-word-or-char
    keyboard-quit
    left-char
    left-word
    mouse-drag-region
    mouse-save-then-kill
    mouse-set-point
    mouse-set-region
    mwheel-scroll
    move-beginning-of-line
    move-end-of-line
    next-error
    next-line
    paredit-backward
    paredit-backward-down
    paredit-backward-up
    paredit-forward
    paredit-forward-down
    paredit-forward-up
    pop-global-mark
    pop-tag-mark
    pop-to-mark-command
    previous-error
    previous-line
    right-char
    right-word
    scroll-down
    scroll-down-command
    scroll-up
    scroll-up-command
    sgml-skip-tag-backward
    sgml-skip-tag-forward
    up-list)
  "Non-Evil commands to initialize to motions."
  :type  '(repeat symbol)
  :group 'vimp
  :set 'vimp-set-custom-motions
  :initialize 'vimp-custom-initialize-pending-reset)

(defcustom vimp-visual-newline-commands
  '(LaTeX-section
    TeX-font)
  "Commands excluding the trailing newline of a Visual Line selection.
These commands work better without this newline."
  :type  '(repeat symbol)
  :group 'vimp
  :set 'vimp-set-visual-newline-commands
  :initialize 'vimp-custom-initialize-pending-reset)

(defcustom vimp-want-visual-char-semi-exclusive nil
  "Visual character selection to beginning/end of line is exclusive.
If non nil then an inclusive visual character selection which
ends at the beginning or end of a line is turned into an
exclusive selection. Thus if the selected (inclusive) range ends
at the beginning of a line it is changed to not include the first
character of that line, and if the selected range ends at the end
of a line it is changed to not include the newline character of
that line."
  :type 'boolean
  :group 'vimp)

(defgroup vimp-cjk nil
  "CJK support"
  :prefix "vimp-cjk-"
  :group 'vimp)

(defcustom vimp-cjk-emacs-word-boundary nil
  "Determine word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'vimp-cjk)

(defcustom vimp-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `vimp-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '((character . character))
  :group 'vimp-cjk)

(defcustom vimp-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `vimp-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '((character . character))
  :group 'vimp-cjk)

(defcustom vimp-ex-complete-emacs-commands 'in-turn
  "TAB-completion for Emacs commands in ex command line.
This variable determines when Emacs commands are considered for
completion, always, never, or only if no Evil ex command is
available for completion."
  :group 'vimp
  :type '(radio (const :tag "Only if no ex-command." :value in-turn)
                (const :tag "Never" :value nil)
                (const :tag "Always" :value t)))

(defface vimp-ex-commands '(( nil
                              :underline t
                              :slant italic))
         "Face for the vimp command in completion in ex mode."
         :group 'vimp)

(defface vimp-ex-info '(( ((supports :slant))
                          :slant italic
                          :foreground "red"))
         "Face for the info message in ex mode."
         :group 'vimp)

(defcustom vimp-ex-visual-char-range nil
  "Type of default ex range in visual char state.
If non-nil the default range when starting an ex command from
character visual state is `<,`> otherwise it is '<,'>. In the
first case the ex command will be passed a region covering only
the visual selection. In the second case the passed region will
be extended to contain full lines."
  :group 'vimp
  :type 'boolean)

;; Searching
(defcustom vimp-symbol-word-search nil
  "If nil then * and # search for words otherwise for symbols."
  :group 'vimp
  :type 'boolean)
(make-variable-buffer-local 'vimp-symbol-word-search)

(defcustom vimp-magic t
  "Meaning which characters in a pattern are magic.
The meaning of those values is the same as in Vim. Note that it
only has influence if the vimp search module is chosen in
`vimp-search-module'."
  :group 'vimp
  :type '(radio (const :tag "Very magic." :value very-magic)
                (const :tag "Magic" :value t)
                (const :tag "Nomagic" :value nil)
                (const :tag "Very nomagic" :value very-nomagic)))

(defcustom vimp-ex-search-vim-style-regexp nil
  "If non-nil Vim-style backslash codes are supported in search patterns.
See `vimp-transform-vim-style-regexp' for the supported backslash
codes.  Note that this only affects the search command if
`vimp-search-module' is set to 'vimp. The isearch module always
uses plain Emacs regular expressions."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-ex-interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'vimp)

(defcustom vimp-ex-search-persistent-highlight t
  "If non-nil matches remained highlighted when the search ends."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-ex-search-case 'smart
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive."
  :type '(radio (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'vimp)

(defcustom vimp-ex-substitute-case nil
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive. If nil then the setting of `vimp-ex-search-case' is
used."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'vimp)

(defcustom vimp-ex-search-interactive t
  "If t search is interactive."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-ex-search-highlight-all t
  "If t and interactive search is enabled, all matches are
highlighted."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-ex-substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-ex-substitute-interactive-replace t
  "If t and substitute patterns are highlighted,
the replacement is shown interactively."
  :type 'boolean
  :group 'vimp)

(defcustom vimp-ex-substitute-global nil
  "If non-nil substitute patterns a global by default.
Usually (if this variable is nil) a substitution works only on
the first match of a pattern in a line unless the 'g' flag is
given, in which case the substitution happens on all matches in a
line. If this option is non-nil, this behaviour is reversed: the
substitution works on all matches unless the 'g' pattern is
specified, then is works only on the first match."
  :type  'boolean
  :group 'vimp)

(defface vimp-ex-search '((t :inherit isearch))
         "Face for interactive search."
         :group 'vimp)

(defface vimp-ex-lazy-highlight '((t :inherit lazy-highlight))
         "Face for highlighting all matches in interactive search."
         :group 'vimp)

(defface vimp-ex-substitute-matches '((t :inherit lazy-highlight))
         "Face for interactive substitute matches."
         :group 'vimp)

(defface vimp-ex-substitute-replacement '((((supports :underline))
                                           :underline t
                                           :foreground "red"))
         "Face for interactive replacement text."
         :group 'vimp)

(defcustom vimp-command-window-height 8
  "Height (in lines) of the command line window.
Set to 0 to use the default height for `split-window'."
  :type 'integer
  :group 'vimp)

(defcustom vimp-display-shell-error-in-message nil
  "Show error output of a shell command in the error buffer.
If this variable is non-nil the error output of a shell command
goes to the messages buffer instead of being mixed with the
regular output. This happens only of the exit status of the
command is non-zero."
  :type 'boolean
  :group 'vimp)

;;; Variables

(defmacro vimp-define-local-var (symbol &optional initvalue docstring)
  "Define SYMBOL as permanent buffer local variable, and return SYMBOL.
The parameters are the same as for `defvar', but the variable
SYMBOL is made permanent buffer local."
  (declare (indent defun)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(vimp-define-local-var vimp-state nil
  "The current Evil state.
To change the state, use `vimp-change-state'
or call the state function (e.g., `vimp-normal-state').")

;; these may be used inside `vimp-define-state'
(vimp-define-local-var vimp-next-state nil
  "The Evil state being switched to.")

(vimp-define-local-var vimp-previous-state-alist nil
  "For Each vimp state the Evil state being switched from.")

(vimp-define-local-var vimp-previous-state nil
  "The Evil state being switched from.")

(defvar vimp-execute-in-emacs-state-buffer nil
  "The buffer of the latest `vimp-execute-in-emacs-state'.
When this command is being executed the current buffer is stored
in this variable. This is necessary in case the Emacs-command to
be called changes the current buffer.")

(vimp-define-local-var vimp-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(put 'vimp-mode-line-tag 'risky-local-variable t)

(defvar vimp-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar vimp-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar vimp-state-properties nil
  "Specifications made by `vimp-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `vimp-state-property'.")

(vimp-define-local-var vimp-mode-map-alist nil
  "Association list of keymaps to use for Evil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")

(defvar vimp-command-properties nil
  "Specifications made by `vimp-define-command'.")

(defvar vimp-transient-vars '(cua-mode transient-mark-mode select-active-regions)
  "List of variables pertaining to Transient Mark mode.")

(defvar vimp-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(vimp-define-local-var vimp-no-display nil
  "If non-nil, various Evil displays are inhibited.
Use the macro `vimp-without-display' to set this variable.")

(defvar vimp-type-properties nil
  "Specifications made by `vimp-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar vimp-interactive-alist nil
  "Association list of Evil-specific interactive codes.")

(vimp-define-local-var vimp-motion-marker nil
  "Marker for storing the starting position of a motion.")

(vimp-define-local-var vimp-this-type nil
  "Current motion type.")

(vimp-define-local-var vimp-this-register nil
  "Current register.")

(vimp-define-local-var vimp-this-macro nil
  "Current macro register.")

(vimp-define-local-var vimp-this-operator nil
  "Current operator.")

(vimp-define-local-var vimp-this-motion nil
  "Current motion.")

(vimp-define-local-var vimp-this-motion-count nil
  "Current motion count.")

(defvar vimp-last-register nil
  "The last executed register.")

(defvar vimp-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar vimp-inhibit-operator-value nil
  "This variable is used to transfer the value
of `vimp-inhibit-operator' from one local scope to another.")

;; used by `vimp-define-operator'
(defvar vimp-operator-range-beginning nil
  "Beginning of `vimp-operator-range'.")

(defvar vimp-operator-range-end nil
  "End of `vimp-operator-range'.")

(defvar vimp-operator-range-type nil
  "Type of `vimp-operator-range'.")

(defvar vimp-operator-range-motion nil
  "Motion of `vimp-operator-range'.")

(defvar vimp-restriction-stack nil
  "List of previous restrictions.
Using `vimp-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

(vimp-define-local-var vimp-markers-alist
  '((?\( . vimp-backward-sentence)
    (?\) . vimp-forward-sentence)
    (?{ . vimp-backward-paragraph)
    (?} . vimp-forward-paragraph)
    (?' . vimp-jump-backward)
    (?` . vimp-jump-backward)
    (?< . vimp-visual-beginning)
    (?> . vimp-visual-goto-end)
    (?. . (lambda ()
            (let (last-command)
              (goto-last-change nil)))))
  "Association list for markers.
Entries have the form (CHAR . DATA), where CHAR is the marker's
name and DATA is either a marker object as returned by `make-marker',
a variable, a movement function, or a cons cell (STRING NUMBER),
where STRING is a file path and NUMBER is a buffer position.
The global value of this variable holds markers available from
every buffer, while the buffer-local value holds markers available
only in the current buffer.")

(vimp-define-local-var vimp-jump-list nil
  "Jump list.")

(defconst vimp-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap vimp-suppress-map t)

(defvar vimp-read-key-map (make-sparse-keymap)
  "Keymap active during `vimp-read-key'.
This keymap can be used to bind some commands during the
execution of `vimp-read-key' which is usually used to read a
character argument for some commands, e.g. `vimp-replace'.")

;; TODO: customize size of ring
(defvar vimp-repeat-ring (make-ring 10)
  "A ring of repeat-informations to repeat the last command.")

(defvar vimp-repeat-types
  '((t . vimp-repeat-keystrokes)
    (change . vimp-repeat-changes)
    (motion . vimp-repeat-motion)
    (insert-at-point . vimp-repeat-insert-at-point)
    (ignore . nil))
  "An alist of defined repeat-types.")

(defvar vimp-recording-repeat nil
  "Whether we are recording a repeat.")

(defvar vimp-recording-current-command nil
  "Whether we are recording the current command for repeat.")

(defvar vimp-repeat-changes nil
  "Accumulated buffer changes for changed-based commands.")

(defvar vimp-repeat-info nil
  "Information accumulated during current repeat.")

(defvar vimp-repeat-buffer nil
  "The buffer in which the repeat started.
If the buffer is changed, the repeat is cancelled.")

(defvar vimp-repeat-pos nil
  "The position of point at the beginning of an change-tracking
  editing command.")

(defvar vimp-repeat-keys nil
  "The keys that invoked the current command.")

(defvar vimp-last-repeat nil
  "Information about the latest repeat command.
This is a list of three elements (POINT COUNT UNDO-POINTER),
where POINT is the position of point before the latest repeat,
COUNT the count-argument of the latest repeat command and
UNDO-POINTER the head of the undo-list before the last command
has been repeated.")

(defvar vimp-repeat-count nil
  "The explicit count when repeating a command.")

(vimp-define-local-var vimp-insert-count nil
  "The explicit count passed to an command starting Insert state.")

(vimp-define-local-var vimp-insert-vcount nil
  "The information about the number of following lines the
insertion should be repeated. This is list (LINE COLUMN COUNT)
where LINE is the line-number where the original insertion
started and COLUMN is either a number of function determining the
column where the repeated insertions should take place. COUNT is
number of repeats (including the original insertion).")

(defvar vimp-insert-skip-empty-lines nil
  "Non-nil of the current insertion should not take place on
  lines at which the insertion point is behind the end of the
  line.")

(vimp-define-local-var vimp-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")

(vimp-define-local-var vimp-insert-repeat-info nil
  "Repeat information accumulated during an insertion.")

(vimp-define-local-var vimp-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")

(vimp-define-local-var vimp-echo-area-message nil
  "Previous value of `current-message'.")

(defvar vimp-write-echo-area nil
  "If set to t inside `vimp-save-echo-area', then the echo area
is not restored.")

(defvar vimp-last-find nil
  "A pair (FUNCTION . CHAR) describing the lastest character
  search command.")

(defvar vimp-last-paste nil
  "Information about the latest paste.
This should be a list (CMD COUNT POINT BEG END FIRSTVISUAL) where
CMD is the last paste-command (`vimp-paste-before',
`vimp-paste-after' or `vimp-visual-paste'), COUNT is the repeat
count of the paste, POINT is the position of point before the
paste, BEG end END are the region of the inserted
text. FIRSTVISUAL is t if and only if the previous command was
the first visual paste (i.e. before any paste-pop).")

(vimp-define-local-var vimp-last-undo-entry nil
  "Information about the latest undo entry in the buffer.
This should be a pair (OBJ . CONS) where OBJ is the entry as an
object, and CONS is a copy of the entry.")

(vimp-define-local-var vimp-current-insertion nil
  "Information about the latest insertion in insert state.
This should be a pair (BEG . END) that describes the
buffer-region of the newly inserted text.")

(defvar vimp-last-insertion nil
  "The last piece of inserted text.")

(defvar vimp-last-small-deletion nil
  "The last piece of deleted text.
The text should be less than a line.")

(defvar vimp-was-yanked-without-register t
  "Whether text being saved to the numbered-register ring was
not deleted and not yanked to a specific register.")

(defvar vimp-paste-count nil
  "The count argument of the current paste command.")

(defvar vimp-temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(vimp-define-local-var vimp-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")

(defvar vimp-in-single-undo nil
  "Set to non-nil if the current undo steps are connected.")

(defvar vimp-flash-timer nil
  "Timer for flashing search results.")

(defvar vimp-search-prompt nil
  "String to use for search prompt.")

(defvar vimp-search-forward-history nil
  "History of forward searches.")

(defvar vimp-search-backward-history nil
  "History of backward searches.")

(defvar vimp-inner-text-objects-map (make-sparse-keymap)
  "Keymap for inner text objects.")

(defvar vimp-outer-text-objects-map (make-sparse-keymap)
  "Keymap for outer text objects.")

(defvar vimp-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(vimp-define-local-var vimp-input-method nil
  "Input method used in Insert state and Emacs state.")

;;; Visual state

(vimp-define-local-var vimp-visual-beginning nil
  "The beginning of the Visual selection, a marker.")

(vimp-define-local-var vimp-visual-end nil
  "The end of the Visual selection, a marker.")

(vimp-define-local-var vimp-visual-point nil
  "The position of point in Visual state, a marker.")

(vimp-define-local-var vimp-visual-mark nil
  "The position of mark in Visual state, a marker.")

(vimp-define-local-var vimp-visual-previous-mark nil
  "The position of mark before Visual state, a marker.")

(vimp-define-local-var vimp-visual-selection nil
  "The kind of Visual selection.
This is a selection as defined by `vimp-define-visual-selection'.")

;; we could infer the direction by comparing `vimp-visual-mark'
;; and `vimp-visual-point', but destructive operations may
;; displace the markers
(vimp-define-local-var vimp-visual-direction 0
  "Whether point follows mark in Visual state.
Negative if point precedes mark, otherwise positive.
See also the function `vimp-visual-direction'.")

(vimp-define-local-var vimp-visual-properties nil
  "Property list of miscellaneous Visual properties.")

(vimp-define-local-var vimp-visual-region-expanded nil
  "Whether the region matches the Visual selection.
That is, whether the positions of point and mark have been
expanded to coincide with the selection's boundaries.
This makes the selection available to functions acting
on Emacs' region.")

(vimp-define-local-var vimp-visual-overlay nil
  "Overlay for highlighting the Visual selection.
Not used for blockwise selections, in which case
see `vimp-visual-block-overlays'.")

(vimp-define-local-var vimp-visual-block-overlays nil
  "Overlays for Visual Block selection, one for each line.
They are reused to minimize flicker.")

(defvar vimp-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

(vimp-define-local-var vimp-visual-x-select-timer nil
  "Timer for updating the X selection in visual state.")

(defvar vimp-visual-x-select-timeout 0.1
  "Time in seconds for the update of the X selection.")

(defvar vimp-fold-list
  `(((hs-minor-mode)
     :open-all   hs-show-all
     :close-all  hs-hide-all
     :toggle     hs-toggle-hiding
     :open       hs-show-block
     :open-rec   nil
     :close      hs-hide-block)
    ((hide-ifdef-mode)
     :open-all   show-ifdefs
     :close-all  hide-ifdefs
     :toggle     nil
     :open       show-ifdef-block
     :open-rec   nil
     :close      hide-ifdef-block)
    ((outline-mode
      outline-minor-mode
      org-mode
      markdown-mode)
     :open-all   show-all
     :close-all  ,(lambda ()
                    (with-no-warnings (hide-sublevels 1)))
     :toggle     outline-toggle-children
     :open       ,(lambda ()
                    (with-no-warnings
                      (show-entry)
                      (show-children)))
     :open-rec   show-subtree
     :close      hide-subtree))
  "Actions to be performed for various folding operations.

The value should be a list of fold handlers, were a fold handler has
the format:

  ((MODES) PROPERTIES)

MODES acts as a predicate, containing the symbols of all major or
minor modes for which the handler should match.  For example:

  '((outline-minor-mode org-mode) ...)

would match for either outline-minor-mode or org-mode, even though the
former is a minor mode and the latter is a major.

PROPERTIES specifies possible folding actions and the functions to be
applied in the event of a match on one (or more) of the MODES; the
supported properties are:

  - `:open-all'
    Open all folds.
  - `:close-all'
    Close all folds.
  - `:toggle'
    Toggle the display of the fold at point.
  - `:open'
    Open the fold at point.
  - `:open-rec'
    Open the fold at point recursively.
  - `:close'
    Close the fold at point.

Each value must be a function.  A value of `nil' will cause the action
to be ignored for that respective handler.  For example:

  `((org-mode)
     :close-all  nil
     :open       ,(lambda ()
                    (show-entry)
                    (show-children))
     :close      hide-subtree)

would ignore `:close-all' actions and invoke the provided functions on
`:open' or `:close'.")

;;; Ex

(defvar vimp-ex-map (make-sparse-keymap)
  "Keymap for Ex.
Key sequences bound in this map are immediately executed.")

(defvar vimp-ex-completion-map (make-sparse-keymap)
  "Completion keymap for Ex.")

(defvar vimp-ex-initial-input nil
  "Additional initial content of the ex command line.
This content of this variable is appended to the ex command line
if ex is started interactively.")

(defvar vimp-ex-shell-argument-initialized nil
  "This variable is set to t if shell command completion has been initialized.
See `vimp-ex-init-shell-argument-completion'.")

(defvar vimp-ex-commands nil
  "Association list of command bindings and functions.")

(defvar vimp-ex-history nil
  "History of Ex commands.")

(defvar vimp-ex-current-buffer nil
  "The buffer from which Ex was started.")

(defvar vimp-ex-expression nil
  "The evaluation tree.")

(defvar vimp-ex-tree nil
  "The syntax tree.")

(defvar vimp-ex-command nil
  "The current Ex command.")

(defvar vimp-ex-previous-command nil
  "The previously executed Ex command.")

(defvar vimp-ex-cmd nil
  "The current Ex command string.")

(defvar vimp-ex-point nil
  "The position of `point' when the ex command has been called.")

(defvar vimp-ex-range nil
  "The current range of the Ex command.")

(defvar vimp-ex-bang nil
  "The \"!\" argument of the current Ex command.")

(defvar vimp-ex-argument nil
  "The current argument of the Ex command.")

(defvar vimp-ex-argument-handler nil
  "The argument handler for the current Ex command.")

(defvar vimp-ex-argument-types nil
  "Association list of argument handlers.")

(defvar vimp-previous-shell-command nil
  "The last shell command.")

;; Searching
(defvar vimp-ex-search-history nil
  "The history for the search command.")

(defvar vimp-ex-search-direction nil
  "The direction of the current search, either 'forward or 'backward.")

(defvar vimp-ex-search-count nil
  "The count if the current search.")

(defvar vimp-ex-search-start-point nil
  "The point where the search started.")

(defvar vimp-ex-search-overlay nil
  "The overlay for the current search result.")

(defvar vimp-ex-search-pattern nil
  "The last search pattern.")

(defvar vimp-ex-search-offset nil
  "The last search offset.")

(defvar vimp-ex-search-match-beg nil
  "The beginning position of the last match.")

(defvar vimp-ex-search-match-end nil
  "The end position of the last match.")

(defvar vimp-ex-substitute-pattern nil
  "The last substitute pattern.")

(defvar vimp-ex-substitute-replacement nil
  "The last substitute replacement.")

(defvar vimp-ex-substitute-flags nil
  "The last substitute flags.")

(defvar vimp-ex-substitute-current-replacement nil
  "The actual replacement.")

(defvar vimp-ex-last-was-search nil
  "Non-nil if the previous was a search.
Otherwise the previous command is assumed as substitute.")

;;; Command line window

(defvar vimp-command-window-current-buffer nil
  "The buffer from which the command line window was called.")

(vimp-define-local-var vimp-command-window-execute-fn nil
  "The command to execute when exiting the command line window.")

(vimp-define-local-var vimp-command-window-cmd-key nil
  "The key for the command that opened the command line window (:, /, or ?).")

;; The lazy-highlighting framework.
(vimp-define-local-var vimp-ex-active-highlights-alist nil
  "An alist of currently active highlights.")

(vimp-define-local-var vimp-ex-hl-update-timer nil
  "Time used for updating highlights.")

(defvar vimp-ex-search-keymap (make-sparse-keymap)
  "Keymap used in ex-search-mode.")
(set-keymap-parent vimp-ex-search-keymap minibuffer-local-map)

(defconst vimp-version
  (eval-when-compile
    (with-temp-buffer
      (let ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file))))
        (cond
         ;; git repository
         ((and (file-exists-p (concat dir "/.git"))
               (condition-case nil
                   (zerop (call-process "git" nil '(t nil) nil
                                        "rev-parse"
                                        "--short" "HEAD"))
                 (error nil)))
          (goto-char (point-min))
          (concat "vimp-git-"
                  (buffer-substring (point-min)
                                    (line-end-position))))
         ;; mercurial repository
         ((and (file-exists-p (concat dir "/.hg"))
               (condition-case nil
                   (zerop (call-process "hg" nil '(t nil) nil
                                        "parents"
                                        "--template"
                                        "vimp-hg-{node|short}"))
                 (error nil)))
          (goto-char (point-min))
          (buffer-substring (point-min) (line-end-position)))
         ;; no repo, use plain version
         (t "1.2.5")))))
  "The current version of Evil")

(defun vimp-version ()
  (interactive)
  (message "Evil version %s" vimp-version))

(provide 'vimp-vars)

;;; vimp-vars.el ends here
