;;; vimp-integration.el --- Integrate Evil with other modules

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

(require 'vimp-maps)
(require 'vimp-core)
(require 'vimp-macros)
(require 'vimp-types)
(require 'vimp-repeat)

;;; Code:

;;; Evilize some commands

;; unbound keys should be ignored
(vimp-declare-ignore-repeat 'undefined)

(mapc #'(lambda (cmd)
          (vimp-set-command-property cmd :keep-visual t)
          (vimp-declare-not-repeat cmd))
      '(digit-argument
        negative-argument
        universal-argument
        universal-argument-minus
        universal-argument-more
        universal-argument-other-key))
(mapc #'vimp-declare-not-repeat
      '(what-cursor-position))
(mapc #'vimp-declare-change-repeat
      '(dabbrev-expand
        hippie-expand))
(mapc #'vimp-declare-abort-repeat
      '(balance-windows
        eval-expression
        execute-extended-command
        exit-minibuffer
        compile
        delete-window
        delete-other-windows
        find-file-at-point
        ffap-other-window
        recompile
        redo
        save-buffer
        split-window
        split-window-horizontally
        split-window-vertically
        undo
        undo-tree-redo
        undo-tree-undo))

(vimp-set-type #'previous-line 'line)
(vimp-set-type #'next-line 'line)

(dolist (cmd '(keyboard-quit keyboard-escape-quit))
  (vimp-set-command-property cmd :suppress-operator t))

;;; Mouse
(vimp-declare-insert-at-point-repeat 'mouse-yank-primary)
(vimp-declare-insert-at-point-repeat 'mouse-yank-secondary)

;;; key-binding

;; Calling `keyboard-quit' should cancel repeat
(defadvice keyboard-quit (before vimp activate)
  (when (fboundp 'vimp-repeat-abort)
    (vimp-repeat-abort)))

;; etags-select
;; FIXME: probably etags-select should be recomended in docs
(eval-after-load 'etags-select
  '(progn
     (define-key vimp-motion-state-map "g]" 'etags-select-find-tag-at-point)))

;;; Buffer-menu

(vimp-add-hjkl-bindings Buffer-menu-mode-map 'motion)

;; dictionary.el

(vimp-add-hjkl-bindings dictionary-mode-map 'motion
  "?" 'dictionary-help        ; "h"
  "C-o" 'dictionary-previous) ; "l"

;;; Dired

(eval-after-load 'dired
  '(progn
     ;; use the standard Dired bindings as a base
     (defvar dired-mode-map)
     (vimp-make-overriding-map dired-mode-map 'normal)
     (vimp-add-hjkl-bindings dired-mode-map 'normal
       "J" 'dired-goto-file                   ; "j"
       "K" 'dired-do-kill-lines               ; "k"
       "r" 'dired-do-redisplay                ; "l"
       ;; ":d", ":v", ":s", ":e"
       ";" (lookup-key dired-mode-map ":"))))

(eval-after-load 'wdired
  '(progn
     (add-hook 'wdired-mode-hook #'vimp-change-to-initial-state)
     (defadvice wdired-change-to-dired-mode (after vimp activate)
       (vimp-change-to-initial-state nil t))))

;;; ELP

(eval-after-load 'elp
  '(defadvice elp-results (after vimp activate)
     (vimp-motion-state)))

;;; ERT

(vimp-add-hjkl-bindings ert-results-mode-map 'motion)

;;; Info

(vimp-add-hjkl-bindings Info-mode-map 'motion
  "0" 'vimp-digit-argument-or-vimp-beginning-of-line
  (kbd "\M-h") 'Info-help   ; "h"
  "\C-t" 'Info-history-back ; "l"
  "\C-o" 'Info-history-back
  " " 'Info-scroll-up
  "\C-]" 'Info-follow-nearest-node
  (kbd "DEL") 'Info-scroll-down)

;;; Parentheses

(defadvice show-paren-function (around vimp disable)
  "Match parentheses in Normal state."
  (if (if (memq 'not vimp-highlight-closing-paren-at-point-states)
          (memq vimp-state vimp-highlight-closing-paren-at-point-states)
        (not (memq vimp-state vimp-highlight-closing-paren-at-point-states)))
      ad-do-it
    (let ((pos (point)) syntax narrow)
      (setq pos
            (catch 'end
              (dotimes (var (1+ (* 2 vimp-show-paren-range)))
                (if (zerop (mod var 2))
                    (setq pos (+ pos var))
                  (setq pos (- pos var)))
                (setq syntax (syntax-class (syntax-after pos)))
                (cond
                 ((eq syntax 4)
                  (setq narrow pos)
                  (throw 'end pos))
                 ((eq syntax 5)
                  (throw 'end (1+ pos)))))))
      (if pos
          (save-excursion
            (goto-char pos)
            (save-restriction
              (when narrow
                (narrow-to-region narrow (point-max)))
              ad-do-it))
        ;; prevent the preceding pair from being highlighted
        (dolist (ov '(show-paren--overlay
                      show-paren--overlay-1
                      show-paren-overlay
                      show-paren-overlay-1))
          (let ((ov (and (boundp ov) (symbol-value ov))))
            (when (overlayp ov) (delete-overlay ov))))))))

;;; Speedbar

(vimp-add-hjkl-bindings speedbar-key-map 'motion
  "h" 'backward-char
  "j" 'speedbar-next
  "k" 'speedbar-prev
  "l" 'forward-char
  "i" 'speedbar-item-info
  "r" 'speedbar-refresh
  "u" 'speedbar-up-directory
  "o" 'speedbar-toggle-line-expansion
  (kbd "RET") 'speedbar-edit-line)

;; Ibuffer
(eval-after-load 'ibuffer
  '(progn
     (defvar ibuffer-mode-map)
     (vimp-make-overriding-map ibuffer-mode-map 'normal)
     (vimp-define-key 'normal ibuffer-mode-map
       "j" 'vimp-next-line
       "k" 'vimp-previous-line
       "RET" 'ibuffer-visit-buffer)))

;;; Undo tree
(when (and (require 'undo-tree nil t)
           (fboundp 'global-undo-tree-mode))
  (global-undo-tree-mode 1))

(eval-after-load 'undo-tree
  '(with-no-warnings
     (defun vimp-turn-on-undo-tree-mode ()
       "Enable `undo-tree-mode' if vimp is enabled.
This function enables `undo-tree-mode' when Evil is activated in
some buffer, but only if `global-undo-tree-mode' is also
activated."
       (when (and (boundp 'global-undo-tree-mode)
                  global-undo-tree-mode)
         (undo-tree-mode 1)))

     (add-hook 'vimp-local-mode-hook #'vimp-turn-on-undo-tree-mode)

     (defadvice undo-tree-visualize (after vimp activate)
       "Initialize Evil in the visualization buffer."
       (when vimp-local-mode
         (vimp-initialize-state)))

     (when (fboundp 'undo-tree-visualize)
       (vimp-ex-define-cmd "undol[ist]" 'undo-tree-visualize)
       (vimp-ex-define-cmd "ul" 'undo-tree-visualize))

     (when (boundp 'undo-tree-visualizer-mode-map)
       (define-key undo-tree-visualizer-mode-map
         [remap vimp-backward-char] 'undo-tree-visualize-switch-branch-left)
       (define-key undo-tree-visualizer-mode-map
         [remap vimp-forward-char] 'undo-tree-visualize-switch-branch-right)
       (define-key undo-tree-visualizer-mode-map
         [remap vimp-next-line] 'undo-tree-visualize-redo)
       (define-key undo-tree-visualizer-mode-map
         [remap vimp-previous-line] 'undo-tree-visualize-undo)
       (define-key undo-tree-visualizer-mode-map
         [remap vimp-ret] 'undo-tree-visualizer-set))

     (when (boundp 'undo-tree-visualizer-selection-mode-map)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap vimp-backward-char] 'undo-tree-visualizer-select-left)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap vimp-forward-char] 'undo-tree-visualizer-select-right)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap vimp-next-line] 'undo-tree-visualizer-select-next)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap vimp-previous-line] 'undo-tree-visualizer-select-previous)
       (define-key undo-tree-visualizer-selection-mode-map
         [remap vimp-ret] 'undo-tree-visualizer-set))))

;;; Auto-complete
(eval-after-load 'auto-complete
  '(progn
     (vimp-add-command-properties 'auto-complete :repeat 'vimp-ac-repeat)
     (vimp-add-command-properties 'ac-complete :repeat 'vimp-ac-repeat)
     (vimp-add-command-properties 'ac-expand :repeat 'vimp-ac-repeat)
     (vimp-add-command-properties 'ac-next :repeat 'ignore)
     (vimp-add-command-properties 'ac-previous :repeat 'ignore)

     (defvar vimp-ac-prefix-len nil
       "The length of the prefix of the current item to be completed.")

     (defvar ac-prefix)
     (defun vimp-ac-repeat (flag)
       "Record the changes for auto-completion."
       (cond
        ((eq flag 'pre)
         (setq vimp-ac-prefix-len (length ac-prefix))
         (vimp-repeat-start-record-changes))
        ((eq flag 'post)
         ;; Add change to remove the prefix
         (vimp-repeat-record-change (- vimp-ac-prefix-len)
                                    ""
                                    vimp-ac-prefix-len)
         ;; Add change to insert the full completed text
         (vimp-repeat-record-change
          (- vimp-ac-prefix-len)
          (buffer-substring-no-properties (- vimp-repeat-pos
                                             vimp-ac-prefix-len)
                                          (point))
          0)
         ;; Finish repeation
         (vimp-repeat-finish-record-changes))))))

;;; Company
(eval-after-load 'company
  '(progn
     (mapc #'vimp-declare-change-repeat
           '(company-complete-mouse
             company-complete-number
             company-complete-selection
             company-complete-common))

     (mapc #'vimp-declare-ignore-repeat
           '(company-abort
             company-select-next
             company-select-previous
             company-select-next-or-abort
             company-select-previous-or-abort
             company-select-mouse
             company-show-doc-buffer
             company-show-location
             company-search-candidates
             company-filter-candidates))))

;; Eval last sexp
(cond
 ((version< emacs-version "25")
  (defadvice preceding-sexp (around vimp activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not vimp-move-beyond-eol)
             (or (vimp-normal-state-p) (vimp-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it))

  (defadvice pp-last-sexp (around vimp activate)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not vimp-move-beyond-eol)
             (or (vimp-normal-state-p) (vimp-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          ad-do-it)
      ad-do-it)))
 (t
  (defun vimp--preceding-sexp (command &rest args)
    "In normal-state or motion-state, last sexp ends at point."
    (if (and (not vimp-move-beyond-eol)
             (or (vimp-normal-state-p) (vimp-motion-state-p)))
        (save-excursion
          (unless (or (eobp) (eolp)) (forward-char))
          (apply command args))
      (apply command args)))

  (advice-add 'elisp--preceding-sexp :around 'vimp--preceding-sexp '((name . vimp)))
  (advice-add 'pp-last-sexp          :around 'vimp--preceding-sexp '((name . vimp)))))

;; Show key
(defadvice quail-show-key (around vimp activate)
  "Temporarily go to Emacs state"
  (vimp-with-state emacs ad-do-it))

(defadvice describe-char (around vimp activate)
  "Temporarily go to Emacs state"
  (vimp-with-state emacs ad-do-it))

;; ace-jump-mode
(declare-function 'ace-jump-char-mode "ace-jump-mode")
(declare-function 'ace-jump-word-mode "ace-jump-mode")
(declare-function 'ace-jump-line-mode "ace-jump-mode")

(defvar vimp-ace-jump-active nil)

(defmacro vimp-enclose-ace-jump-for-motion (&rest body)
  "Enclose ace-jump to make it suitable for motions.
This includes restricting `ace-jump-mode' to the current window
in visual and operator state, deactivating visual updates, saving
the mark and entering `recursive-edit'."
  (declare (indent defun)
           (debug t))
  `(let ((old-mark (mark))
         (ace-jump-mode-scope
          (if (and (not (memq vimp-state '(visual operator)))
                   (boundp 'ace-jump-mode-scope))
              ace-jump-mode-scope
            'window)))
     (remove-hook 'pre-command-hook #'vimp-visual-pre-command t)
     (remove-hook 'post-command-hook #'vimp-visual-post-command t)
     (unwind-protect
         (let ((vimp-ace-jump-active 'prepare))
           (add-hook 'ace-jump-mode-end-hook
                     #'vimp-ace-jump-exit-recursive-edit)
           ,@body
           (when vimp-ace-jump-active
             (setq vimp-ace-jump-active t)
             (recursive-edit)))
       (remove-hook 'post-command-hook
                    #'vimp-ace-jump-exit-recursive-edit)
       (remove-hook 'ace-jump-mode-end-hook
                    #'vimp-ace-jump-exit-recursive-edit)
       (if (vimp-visual-state-p)
           (progn
             (add-hook 'pre-command-hook #'vimp-visual-pre-command nil t)
             (add-hook 'post-command-hook #'vimp-visual-post-command nil t)
             (set-mark old-mark))
         (push-mark old-mark)))))

(eval-after-load 'ace-jump-mode
  `(defadvice ace-jump-done (after vimp activate)
     (when vimp-ace-jump-active
       (add-hook 'post-command-hook #'vimp-ace-jump-exit-recursive-edit))))

(defun vimp-ace-jump-exit-recursive-edit ()
  "Exit a recursive edit caused by an vimp jump."
  (cond
   ((eq vimp-ace-jump-active 'prepare)
    (setq vimp-ace-jump-active nil))
   (vimp-ace-jump-active
    (remove-hook 'post-command-hook #'vimp-ace-jump-exit-recursive-edit)
    (exit-recursive-edit))))

(vimp-define-motion vimp-ace-jump-char-mode (count)
  "Jump visually directly to a char using ace-jump."
  :type inclusive
  (vimp-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (vimp-enclose-ace-jump-for-motion
        (call-interactively 'ace-jump-char-mode))
      ;; if we jump backwards, motion type is exclusive, analogously
      ;; to `vimp-find-char-backward'
      (when (and (equal buf (current-buffer))
                 (< (point) pnt))
        (setq vimp-this-type
              (cond
               ((eq vimp-this-type 'exclusive) 'inclusive)
               ((eq vimp-this-type 'inclusive) 'exclusive)))))))

(vimp-define-motion vimp-ace-jump-char-to-mode (count)
  "Jump visually to the char in front of a char using ace-jump."
  :type inclusive
  (vimp-without-repeat
    (let ((pnt (point))
          (buf (current-buffer)))
      (vimp-enclose-ace-jump-for-motion
        (call-interactively 'ace-jump-char-mode))
      (if (and (equal buf (current-buffer))
               (< (point) pnt))
          (progn
            (or (eobp) (forward-char))
            (setq vimp-this-type
                  (cond
                   ((eq vimp-this-type 'exclusive) 'inclusive)
                   ((eq vimp-this-type 'inclusive) 'exclusive))))
        (backward-char)))))

(vimp-define-motion vimp-ace-jump-line-mode (count)
  "Jump visually to the beginning of a line using ace-jump."
  :type line
  :repeat abort
  (vimp-without-repeat
    (vimp-enclose-ace-jump-for-motion
      (call-interactively 'ace-jump-line-mode))))

(vimp-define-motion vimp-ace-jump-word-mode (count)
  "Jump visually to the beginning of a word using ace-jump."
  :type exclusive
  :repeat abort
  (vimp-without-repeat
    (vimp-enclose-ace-jump-for-motion
      (call-interactively 'ace-jump-word-mode))))

(define-key vimp-motion-state-map [remap ace-jump-char-mode] #'vimp-ace-jump-char-mode)
(define-key vimp-motion-state-map [remap ace-jump-line-mode] #'vimp-ace-jump-line-mode)
(define-key vimp-motion-state-map [remap ace-jump-word-mode] #'vimp-ace-jump-word-mode)

;;; avy
(declare-function 'avy-goto-word-or-subword-1 "avy")
(declare-function 'avy-goto-line "avy")
(declare-function 'avy-goto-char "avy")
(declare-function 'avy-goto-char-2 "avy")
(declare-function 'avy-goto-word-0 "avy")
(declare-function 'avy-goto-word-1 "avy")
(declare-function 'avy-goto-subword-0 "avy")
(declare-function 'avy-goto-subword-1 "avy")

(defmacro vimp-enclose-avy-for-motion (&rest body)
  "Enclose avy to make it suitable for motions.
Based on `vimp-enclose-ace-jump-for-motion'."
  (declare (indent defun)
           (debug t))
  `(let ((avy-all-windows
          (if (and (not (memq vimp-state '(visual operator)))
                   (boundp 'avy-all-windows))
              avy-all-windows
            nil)))
     ,@body))

(defmacro vimp-define-avy-motion (command type)
  (declare (indent defun)
           (debug t))
  (let ((name (intern (format "vimp-%s" command))))
    `(vimp-define-motion ,name (_count)
       ,(format "Evil motion for `%s'." command)
       :type ,type
       :jump t
       :repeat abort
       (vimp-without-repeat
         (vimp-enclose-avy-for-motion
           (call-interactively ',command))))))

;; define vimp-avy-* motion commands for avy-* commands
(vimp-define-avy-motion avy-goto-word-or-subword-1 exclusive)
(vimp-define-avy-motion avy-goto-line line)
(vimp-define-avy-motion avy-goto-char inclusive)
(vimp-define-avy-motion avy-goto-char-2 inclusive)
(vimp-define-avy-motion avy-goto-word-0 exclusive)
(vimp-define-avy-motion avy-goto-word-1 exclusive)
(vimp-define-avy-motion avy-goto-subword-0 exclusive)
(vimp-define-avy-motion avy-goto-subword-1 exclusive)

;; remap avy-* commands to vimp-avy-* commands
(dolist (command '(avy-goto-word-or-subword-1
                   avy-goto-line
                   avy-goto-char
                   avy-goto-char-2
                   avy-goto-word-0
                   avy-goto-word-1
                   avy-goto-subword-0
                   avy-goto-subword-1))
  (define-key vimp-motion-state-map
    (vector 'remap command) (intern-soft (format "vimp-%s" command))))

;;; nXhtml/mumamo
;; ensure that mumamo does not toggle vimp through its globalized mode
(eval-after-load 'mumamo
  '(with-no-warnings
     (push 'vimp-mode-cmhh mumamo-change-major-mode-no-nos)))

;;; ag.el
(eval-after-load 'ag
  '(progn
     (defvar ag-mode-map)
     (add-to-list 'vimp-motion-state-modes 'ag-mode)
     (vimp-add-hjkl-bindings ag-mode-map 'motion)))

(provide 'vimp-integration)

;;; vimp-integration.el ends here
