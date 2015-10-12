;;; vimp-command-window.el --- Evil command line window implementation
;; Author: Emanuel Evans <emanuel.evans at gmail.com>
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

;;; Commentary:

;; This provides an implementation of the vim command line window for
;; editing and repeating past ex commands and searches.

;;; Code:

(require 'vimp-vars)
(require 'vimp-common)
(require 'vimp-search)
(require 'vimp-ex)

(define-derived-mode vimp-command-window-mode fundamental-mode "Evil-cmd"
  "Major mode for the Evil command line window."
  (auto-fill-mode 0)
  (setq-local after-change-functions (cons 'vimp-command-window-draw-prefix
                                           after-change-functions)))

(defun vimp-command-window (hist cmd-key execute-fn)
  "Open a command line window for HIST with CMD-KEY and EXECUTE-FN.
HIST should be a list of commands.  CMD-KEY should be the string of
the key whose history is being shown (one of \":\", \"/\", or
\"?\").  EXECUTE-FN should be a function of one argument to
execute on the result that the user selects."
  (when (eq major-mode 'vimp-command-window-mode)
    (user-error "Cannot recursively open command line window"))
  (mapc #'(lambda (win)
            (when (equal (buffer-name (window-buffer win))
                         "*Command Line*")
              (kill-buffer (window-buffer win))
              (delete-window win)))
        (window-list))
  (split-window nil
                (unless (zerop vimp-command-window-height)
                  vimp-command-window-height)
                'above)
  (setq vimp-command-window-current-buffer (current-buffer))
  (ignore-errors (kill-buffer "*Command Line*"))
  (switch-to-buffer "*Command Line*")
  (vimp-command-window-mode)
  (setq-local vimp-command-window-execute-fn execute-fn)
  (setq-local vimp-command-window-cmd-key cmd-key)
  (vimp-command-window-insert-commands hist))

(defun vimp-command-window-ex (&optional current-command)
  "Open a command line window for editing and executing ex commands.
If CURRENT-COMMAND is present, it will be inserted under the
cursor as the current command to be edited."
  (interactive)
  (vimp-command-window (cons (or current-command " ") vimp-ex-history)
                       ":"
                       'vimp-command-window-ex-execute))

(defun vimp-command-window-execute ()
  "Execute the command under the cursor in the appropriate buffer.
The local var `vimp-command-window-execute-fn' determines which
function to execute."
  (interactive)
  (let ((result (buffer-substring (line-beginning-position)
                                  (line-end-position)))
        (execute-fn vimp-command-window-execute-fn)
        (command-window (get-buffer-window)))
    (select-window (previous-window))
    (unless (equal vimp-command-window-current-buffer (current-buffer))
      (user-error "Originating buffer is no longer active"))
    (kill-buffer "*Command Line*")
    (delete-window command-window)
    (funcall execute-fn result)
    (setq vimp-command-window-current-buffer nil)))

(defun vimp-command-window-ex-execute (result)
  "Execute RESULT as an ex command in the appropriate buffer."
  (unless (string-match-p "^ *$" result)
    (let ((vimp-ex-current-buffer vimp-command-window-current-buffer))
      (vimp-ex-execute result))
    (unless (equal result (car vimp-ex-history))
      (setq vimp-ex-history (cons result vimp-ex-history)))))

(defun vimp-command-window-search-forward ()
  "Open a command line window for forward searches."
  (interactive)
  (vimp-command-window (cons " " vimp-search-forward-history)
                       "/"
                       (lambda (result)
                         (vimp-command-window-search-execute result t))))

(defun vimp-command-window-search-backward ()
  "Open a command line window for backward searches."
  (interactive)
  (vimp-command-window (cons " " vimp-search-backward-history)
                       "?"
                       (lambda (result)
                         (vimp-command-window-search-execute result nil))))

(defun vimp-command-window-search-execute (result forward)
  "Search for RESULT using FORWARD to determine direction."
  (unless (equal result " ")
    (if (and (boundp 'vimp-search-module) (eq vimp-search-module 'vimp-search))
        (progn
          (setq vimp-ex-search-pattern (vimp-ex-make-search-pattern result)
                vimp-ex-search-direction (if forward 'forward 'backward))
          (vimp-ex-search))
      (vimp-search result forward vimp-regexp-search))))

(defun vimp-command-window-draw-prefix (&rest ignored)
  "Display `vimp-command-window-cmd-key' as a prefix to the current line.
Parameters passed in through IGNORED are ignored."
  (let ((prefix (propertize vimp-command-window-cmd-key
                            'font-lock-face 'minibuffer-prompt)))
    (set-text-properties (line-beginning-position) (line-end-position)
                         (list 'line-prefix prefix))))

(defun vimp-command-window-insert-commands (hist)
  "Insert the commands in HIST."
  (let ((inhibit-modification-hooks t))
    (mapc #'(lambda (cmd) (insert cmd) (newline)) hist)
    (join-line)
    (delete-char 1))
  (reverse-region (point-min) (point-max)) ; draws prefixes as a side-effect
  (goto-char (point-max))
  (vimp-adjust-cursor))

(provide 'vimp-command-window)

;;; vimp-command-window.el ends here
