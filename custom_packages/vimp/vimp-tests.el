;; vimp-tests.el --- unit tests for Evil -*- coding: utf-8 -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
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

;; This file is for developers. It runs some tests on Evil.
;; To load it, run the Makefile target "make test" or add
;; the following lines to .emacs:
;;
;;     (setq vimp-tests-run nil) ; set to t to run tests immediately
;;     (global-set-key [f12] 'vimp-tests-run) ; hotkey
;;     (require 'vimp-tests)
;;
;; Loading this file enables profiling on Evil. The current numbers
;; can be displayed with `elp-results'. The Makefile target
;; "make profiler" shows profiling results in the terminal on the
;; basis of running all tests.
;;
;; To write a test, use `ert-deftest' and specify a :tags value of at
;; least '(vimp). The test may inspect the output of functions given
;; certain input, or it may execute a key sequence in a temporary
;; buffer and investigate the results. For the latter approach, the
;; macro `vimp-test-buffer' creates a temporary buffer in Normal
;; state. String descriptors initialize and match the contents of
;; the buffer:
;;
;;     (ert-deftest vimp-test ()
;;       :tags '(vimp)
;;       (vimp-test-buffer
;;        "[T]his creates a test buffer." ; cursor on "T"
;;        ("w")                           ; key sequence
;;        "This [c]reates a test buffer."))) ; cursor moved to "c"
;;
;; The initial state, the cursor syntax, etc., can be changed
;; with keyword arguments. See the documentation string of
;; `vimp-test-buffer' for more details.
;;
;; This file is NOT part of Evil itself.

(require 'elp)
(require 'ert)
(require 'vimp)

;;; Code:

(defvar vimp-tests-run nil
  "*Run Evil tests.")

(defvar vimp-tests-profiler nil
  "*Profile Evil tests.")

(defun vimp-tests-initialize (&optional tests profiler interactive)
  (setq profiler (or profiler vimp-tests-profiler))
  (when (listp profiler)
    (setq profiler (car profiler)))
  (when profiler
    (setq vimp-tests-profiler t)
    (setq profiler
          (or (cdr (assq profiler
                         '((call . elp-sort-by-call-count)
                           (average . elp-sort-by-average-time)
                           (total . elp-sort-by-total-time))))))
    (setq elp-sort-by-function (or profiler 'elp-sort-by-call-count))
    (elp-instrument-package "vimp"))
  (if interactive
      (if (y-or-n-p-with-timeout "Run tests? " 2 t)
          (vimp-tests-run tests interactive)
        (message "You can run the tests at any time \
with `M-x vimp-tests-run'"))
    (vimp-tests-run tests)))

(defun vimp-tests-run (&optional tests interactive)
  "Run Evil tests."
  (interactive '(nil t))
  (let ((elp-use-standard-output (not interactive)))
    (setq tests
          (or (null tests)
              `(or ,@(mapcar #'(lambda (test)
                                 (or (null test)
                                     (and (memq test '(vimp t)) t)
                                     `(or (tag ,test)
                                          ,(format "^%s$" test))))
                             tests))))
    (cond
     (interactive
      (ert-run-tests-interactively tests)
      (when vimp-tests-profiler
        (elp-results)))
     (vimp-tests-profiler
      (ert-run-tests-batch tests)
      (elp-results))
     (t
      (ert-run-tests-batch-and-exit tests)))))

(defun vimp-tests-profiler (&optional force)
  "Profile Evil tests."
  (when (or vimp-tests-profiler force)
    (setq vimp-tests-profiler t)
    (elp-instrument-package "vimp")))

(defvar vimp-test-point nil
  "Marker for point.")
(make-variable-buffer-local 'vimp-test-point)
(defvar vimp-test-visual-start nil
  "Marker for Visual beginning.")
(make-variable-buffer-local 'vimp-test-visual-start)
(defvar vimp-test-visual-end nil
  "Marker for Visual end.")
(make-variable-buffer-local 'vimp-test-visual-end)

(defmacro vimp-test-buffer (&rest body)
  "Execute FORMS in a temporary buffer.
The following optional keywords specify the buffer's properties:

:state STATE            The initial state, defaults to `normal'.
:visual SELECTION       The Visual selection, defaults to `char'.
:point-start STRING     String for matching beginning of point,
                        defaults to \"[\".
:point-end STRING       String for matching end of point,
                        defaults to \"]\".
:visual-start STRING    String for matching beginning of
                        Visual selection, defaults to \"<\".
:visual-end STRING      String for matching end of
                        Visual selection, defaults to \">\".

Then follows one or more forms. If the first form is a string,
it is taken to be a buffer description as passed to
`vimp-test-buffer-from-string', and initializes the buffer.
Subsequent string forms validate the buffer.

If a form is a list of strings or vectors, it is taken to be a
key sequence and is passed to `execute-kbd-macro'.  If the form
is \(file FILENAME CONTENTS), then the test fails unless the
contents of FILENAME equal CONTENTS.  If the form is \(error
SYMBOL ...) then the test fails unless an error of type SYMBOL is
raised.  Remaining forms are evaluated as-is.

\(fn [[KEY VALUE]...] FORMS...)"
  (declare (indent defun))
  (let ((state 'normal)
        arg key point-start point-end string
        visual visual-start visual-end)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :point-start)
        (setq point-start (or arg "")))
       ((eq key :point-end)
        (setq point-end (or arg "")))
       ((eq key :state)
        (setq state arg))
       ((eq key :visual)
        (setq visual arg))
       ((eq key :visual-start)
        (setq visual-start (or arg "")))
       ((eq key :visual-end)
        (setq visual-end (or arg "")))))
    ;; collect buffer initialization
    (when (stringp (car-safe body))
      (setq string (pop body)))
    ;; macro expansion
    `(let ((buffer (vimp-test-buffer-from-string
                    ,string ',state
                    ,point-start ,point-end
                    ',visual ,visual-start ,visual-end))
           (kill-ring kill-ring)
           (kill-ring-yank-pointer kill-ring-yank-pointer)
           x-select-enable-clipboard
           message-log-max)
       (unwind-protect
           (save-window-excursion
             (with-current-buffer buffer
               ;; necessary for keyboard macros to work
               (switch-to-buffer-other-window (current-buffer))
               (buffer-enable-undo)
               (undo-tree-mode 1)
               ;; parse remaining forms
               ,@(mapcar
                  #'(lambda (form)
                      (let (error-symbol)
                        (when (and (listp form)
                                   (eq (car-safe form) 'error))
                          (setq error-symbol (car-safe (cdr-safe form))
                                form (cdr-safe (cdr-safe form))))
                        (let ((result
                               (cond
                                ((stringp form)
                                 `(vimp-test-buffer-string
                                   ,form
                                   ',point-start ',point-end
                                   ',visual-start ',visual-end))
                                ((eq (car-safe form) 'file)
                                 `(vimp-test-file-contents ,(cadr form)
                                                           ,(caddr form)))
                                ((or (stringp (car-safe form))
                                     (vectorp (car-safe form))
                                     (memq (car-safe (car-safe form))
                                           '(kbd vconcat)))
                                 ;; we need to execute everything as a single
                                 ;; sequence for command loop hooks to work
                                 `(execute-kbd-macro
                                   (apply #'vconcat
                                          (mapcar #'listify-key-sequence
                                                  (mapcar #'eval ',form)))))
                                ((memq (car-safe form) '(kbd vconcat))
                                 `(execute-kbd-macro ,form))
                                (t
                                 form))))
                          (if error-symbol
                              `(should-error ,result :type ',error-symbol)
                            result))))
                  body)))
         (and (buffer-name buffer)
              (kill-buffer buffer))))))

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords 'emacs-lisp-mode
                          '(("(\\(vimp-test-buffer\\)\\>"
                             1 font-lock-keyword-face))))

(defun vimp-test-buffer-string (string &optional
                                       point-start point-end
                                       visual-start visual-end)
  "Validate the current buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, that position is compared against point.
If STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, those positions are compared against the Visual selection.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >."
  (let ((actual-buffer (current-buffer))
        (marker-buffer (vimp-test-marker-buffer-from-string
                        string
                        point-start point-end
                        visual-start visual-end))
        before-point after-point string selection)
    (unwind-protect
        (with-current-buffer marker-buffer
          (setq string (buffer-string))
          (when vimp-test-point
            (setq before-point (buffer-substring (point-min) vimp-test-point)
                  after-point (buffer-substring vimp-test-point (point-max))))
          (when (and vimp-test-visual-start vimp-test-visual-end)
            (setq selection (buffer-substring
                             vimp-test-visual-start vimp-test-visual-end)))
          (with-current-buffer actual-buffer
            (if (or before-point after-point)
                (vimp-test-text before-point after-point)
              ;; if the cursor isn't specified, just test the whole buffer
              (save-excursion
                (goto-char (point-min))
                (vimp-test-text nil string #'bobp #'eobp)))
            (when selection
              (vimp-test-selection selection))))
      (kill-buffer marker-buffer))))

(defun vimp-test-buffer-from-string (string &optional
                                            state
                                            point-start point-end
                                            visual visual-start visual-end)
  "Create a new buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, then point is moved to that position.
If STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, then a Visual selection is created with those boundaries.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >.
STATE is the initial state; it defaults to `normal'.
VISUAL is the Visual selection: it defaults to `char'."
  (let ((type (vimp-visual-type (or visual 'char)))
        (buffer (vimp-test-marker-buffer-from-string
                 string point-start point-end
                 visual-start visual-end)))
    (with-current-buffer buffer
      (prog1 buffer
        (vimp-change-state state)
        ;; let the buffer change its major mode without disabling Evil
        (add-hook 'after-change-major-mode-hook #'vimp-initialize)
        (when (and (markerp vimp-test-visual-start)
                   (markerp vimp-test-visual-end))
          (vimp-visual-select
           vimp-test-visual-start vimp-test-visual-end type)
          (when vimp-test-point
            (goto-char vimp-test-point)
            (vimp-visual-refresh)
            (unless (and (= vimp-visual-beginning
                            vimp-test-visual-start)
                         (= vimp-visual-end
                            vimp-test-visual-end))
              (vimp-visual-select
               vimp-test-visual-start vimp-test-visual-end type -1)
              (goto-char vimp-test-point)
              (vimp-visual-refresh))))
        (when (markerp vimp-test-point)
          (goto-char vimp-test-point))))))

(defun vimp-test-marker-buffer-from-string (string &optional
                                                   point-start point-end
                                                   visual-start visual-end)
  "Create a new marker buffer according to STRING.
If STRING contains an occurrence of POINT-START immediately
followed by POINT-END, that position is stored in the
buffer-local variable `vimp-test-point'. Similarly,
if STRING contains an occurrence of VISUAL-START followed by
VISUAL-END, those positions are stored in the variables
`vimp-test-visual-beginning' and `vimp-test-visual-end'.
POINT-START and POINT-END default to [ and ].
VISUAL-START and VISUAL-END default to < and >."
  (let ((string (or string ""))
        (point-start (regexp-quote
                      (if (characterp point-start)
                          (string point-start)
                        (or point-start "["))))
        (point-end (regexp-quote
                    (if (characterp point-end)
                        (string point-end)
                      (or point-end "]"))))
        (visual-start (regexp-quote
                       (if (characterp visual-start)
                           (string visual-start)
                         (or visual-start "<"))))
        (visual-end (regexp-quote
                     (if (characterp visual-end)
                         (string visual-end)
                       (or visual-end ">")))))
    (with-current-buffer (generate-new-buffer " *test*")
      (prog1 (current-buffer)
        (save-excursion
          (insert string))
        (save-excursion
          (when (> (length point-start) 0)
            (if (> (length point-end) 0)
                (when (re-search-forward
                       (format "\\(%s\\)[^%s]?\\(%s\\)"
                               point-start point-end point-end) nil t)
                  (goto-char (match-beginning 0))
                  (delete-region (match-beginning 2) (match-end 2))
                  (delete-region (match-beginning 1) (match-end 1))
                  (setq vimp-test-point
                        (move-marker (make-marker) (point))))
              (when (re-search-forward point-start nil t)
                (goto-char (match-beginning 0))
                (delete-region (match-beginning 0) (match-end 0))
                (setq vimp-test-point
                      (move-marker (make-marker) (point)))))))
        (save-excursion
          (when (and (> (length visual-start) 0)
                     (> (length visual-end) 0))
            (when (re-search-forward visual-start nil t)
              (goto-char (match-beginning 0))
              (delete-region (match-beginning 0) (match-end 0))
              (setq vimp-test-visual-start
                    (move-marker (make-marker) (point))))
            (when (re-search-forward visual-end nil t)
              (goto-char (match-beginning 0))
              (delete-region (match-beginning 0) (match-end 0))
              (setq vimp-test-visual-end
                    (move-marker (make-marker) (point))))))))))

(defun vimp-test-text (before after &optional before-predicate after-predicate)
  "Verify the text around point.
BEFORE is the expected text before point, and AFTER is
the text after point. BEFORE-PREDICATE is a predicate function
to execute at the beginning of the text, and AFTER-PREDICATE
is executed at the end."
  (when before
    (if (functionp before)
        (setq before-predicate before
              before nil)
      (should (string= (buffer-substring
                        (max (point-min) (- (point) (length before)))
                        (point))
                       before))))
  (when after
    (if (functionp after)
        (setq after-predicate after
              after nil)
      (should (string= (buffer-substring
                        (point)
                        (min (point-max) (+ (point) (length after))))
                       after))))
  (when before-predicate
    (ert-info ((format "Expect `%s' at the beginning" before-predicate))
      (save-excursion
        (backward-char (length before))
        (should (funcall before-predicate)))))
  (when after-predicate
    (ert-info ((format "Expect `%s' at the end" after-predicate))
      (save-excursion
        (forward-char (length after))
        (should (funcall after-predicate))))))

(defmacro vimp-test-selection (string &optional end-string
                                      before-predicate after-predicate)
  "Verify that the Visual selection contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (or vimp-visual-beginning (region-beginning)))
       (vimp-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (or vimp-visual-end (region-end)))
       (vimp-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro vimp-test-region (string &optional end-string
                                   before-predicate after-predicate)
  "Verify that the region contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (region-beginning))
       (vimp-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (region-end))
       (vimp-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defmacro vimp-test-overlay (overlay string &optional end-string
                                     before-predicate after-predicate)
  "Verify that OVERLAY contains STRING."
  (declare (indent defun))
  `(progn
     (save-excursion
       (goto-char (overlay-start ,overlay))
       (vimp-test-text nil (or ,string ,end-string) ,before-predicate))
     (save-excursion
       (goto-char (overlay-end ,overlay))
       (vimp-test-text (or ,end-string ,string) nil nil ,after-predicate))))

(defun vimp-temp-filename ()
  "Return an appropriate temporary filename."
  (make-temp-name (expand-file-name "vimp-test"
                                    temporary-file-directory)))

(defmacro vimp-with-temp-file (file-var content &rest body)
  "Create a temp file with CONTENT and bind its name to FILE-VAR within BODY.
FILE-VAR must be a symbol which contains the name of the
temporary file within the macro body. CONTENT is either a string
to be used as the content of the temporary file or a form to be
executed with the temporary file's buffer as \(current-buffer),
see `with-temp-file'. BODY contains the forms to be executed
while the temporary file exists. The temporary file is deleted at
the end of the execution of BODY."
  (declare (indent 2)
           (debug (symbolp form body)))
  `(let ((,file-var (vimp-temp-filename)))
     (with-temp-file ,file-var
       ,(if (stringp content)
            `(insert ,content)
          content))
     ,@body
     (delete-file ,file-var)))

(defun vimp-test-file-contents (name contents)
  "Ensure that the contents of file with NAME equal CONTENTS."
  (with-temp-buffer
    (insert-file-contents name)
    (should (string= (buffer-string)
                     contents))))

;;; States

(defun vimp-test-local-mode-enabled ()
  "Verify that `vimp-local-mode' is enabled properly"
  (ert-info ("Set the mode variable to t")
    (should (eq vimp-local-mode t)))
  (ert-info ("Refresh `emulation-mode-map-alist'")
    (should (memq 'vimp-mode-map-alist emulation-mode-map-alists)))
  (ert-info ("Create a buffer-local value for `vimp-mode-map-alist'")
    (should (assq 'vimp-mode-map-alist (buffer-local-variables))))
  (ert-info ("Initialize buffer-local keymaps")
    (should (assq 'vimp-normal-state-local-map (buffer-local-variables)))
    (should (keymapp vimp-normal-state-local-map))
    (should (assq 'vimp-emacs-state-local-map (buffer-local-variables)))
    (should (keymapp vimp-emacs-state-local-map)))
  (ert-info ("Don't add buffer-local entries to the default value")
    (should-not (rassq vimp-normal-state-local-map
                       (default-value 'vimp-mode-map-alist)))
    (should-not (rassq vimp-emacs-state-local-map
                       (default-value 'vimp-mode-map-alist)))))

(defun vimp-test-local-mode-disabled ()
  "Verify that `vimp-local-mode' is disabled properly"
  (ert-info ("Set the mode variable to nil")
    (should-not vimp-local-mode))
  (ert-info ("Disable all states")
    (vimp-test-no-states)))

(defun vimp-test-no-states ()
  "Verify that all states are disabled"
  (ert-info ("Set `vimp-state' to nil")
    (should-not vimp-state))
  (ert-info ("Disable all state keymaps")
    (dolist (state (mapcar #'car vimp-state-properties) t)
      (should-not (vimp-state-property state :mode t))
      (should-not (memq (vimp-state-property state :keymap t)
                        (current-active-maps)))
      (should-not (vimp-state-property state :local t))
      (should-not (memq (vimp-state-property state :local-keymap t)
                        (current-active-maps)))
      (dolist (map (vimp-state-auxiliary-keymaps state))
        (should-not (memq map (current-active-maps)))))))

(ert-deftest vimp-test-toggle-local-mode ()
  "Toggle `vimp-local-mode'"
  :tags '(vimp state)
  (with-temp-buffer
    (ert-info ("Enable `vimp-local-mode'")
      (vimp-local-mode 1)
      (vimp-test-local-mode-enabled))
    (ert-info ("Disable `vimp-local-mode'")
      (vimp-local-mode -1)
      (vimp-test-local-mode-disabled))))

(defun vimp-test-change-state (state)
  "Change state to STATE and check keymaps"
  (let (mode keymap local-mode local-keymap tag)
    (vimp-change-state state)
    (setq mode (vimp-state-property state :mode)
          keymap (vimp-state-property state :keymap t)
          local-mode (vimp-state-property state :local)
          local-keymap (vimp-state-property state :local-keymap t)
          tag (vimp-state-property state :tag t))
    (ert-info ("Update `vimp-state'")
      (should (eq vimp-state state)))
    (ert-info ("Ensure `vimp-local-mode' is enabled")
      (vimp-test-local-mode-enabled))
    (ert-info ("Enable state modes")
      (should (symbol-value mode))
      (should (symbol-value local-mode)))
    (ert-info ("Push state keymaps to the top")
      (vimp-test-state-keymaps state))
    (ert-info ("Refresh mode line tag")
      (should (equal vimp-mode-line-tag tag)))))

(defun vimp-test-state-keymaps (state)
  "Verify that STATE's keymaps are pushed to the top"
  (let ((actual (vimp-state-keymaps state))
        (expected `((,(vimp-state-property state :local)
                     . , (vimp-state-property state :local-keymap t))
                    (,(vimp-state-property state :mode)
                     . ,(vimp-state-property state :keymap t)))))
    ;; additional keymaps inherited with :enable
    (cond
     ((eq state 'operator)
      (setq expected
            `((vimp-operator-shortcut-mode
               . ,vimp-operator-shortcut-map)
              (vimp-operator-state-local-minor-mode
               . ,vimp-operator-state-local-map)
              (vimp-operator-state-minor-mode
               . ,vimp-operator-state-map)
              (vimp-motion-state-local-minor-mode
               . ,vimp-motion-state-local-map)
              (vimp-motion-state-minor-mode
               . ,vimp-motion-state-map)
              (vimp-normal-state-local-minor-mode
               . ,vimp-normal-state-local-map)
              (vimp-normal-state-minor-mode
               . ,vimp-normal-state-map)))))
    (let ((actual (butlast actual (- (length actual)
                                     (length expected)))))
      (should (equal actual expected))
      (dolist (map actual)
        (setq map (cdr-safe map))
        (should (keymapp map))))))

(ert-deftest vimp-test-exit-normal-state ()
  "Enter Normal state and then disable all states"
  :tags '(vimp state)
  (with-temp-buffer
    (vimp-test-change-state 'normal)
    (vimp-normal-state -1)
    (vimp-test-no-states)))

(ert-deftest vimp-test-change-states ()
  "Change between Normal state, Emacs state and Operator-Pending state"
  :tags '(vimp state)
  (with-temp-buffer
    (vimp-test-change-state 'normal)
    (vimp-test-change-state 'emacs)
    (vimp-test-change-state 'normal)
    (vimp-test-change-state 'operator)
    (vimp-test-change-state 'normal)
    (vimp-test-change-state 'emacs)
    (vimp-test-change-state 'replace)
    (vimp-test-change-state 'normal)))

(ert-deftest vimp-test-change-to-previous-state ()
  "Change to some state and back."
  :tags '(vimp state)
  (with-temp-buffer
    (vimp-test-change-state 'normal)
    (vimp-test-change-state 'visual)
    (vimp-test-change-state 'emacs)
    (vimp-change-to-previous-state)
    (should (eq vimp-state 'visual))
    (vimp-change-to-previous-state)
    (should (eq vimp-state 'normal))))

(ert-deftest vimp-test-enter-normal-state-disabled ()
  "Enter Normal state even if `vimp-local-mode' is disabled"
  :tags '(vimp state)
  (with-temp-buffer
    (vimp-local-mode -1)
    (vimp-test-local-mode-disabled)
    (vimp-test-change-state 'normal)))

(ert-deftest vimp-test-execute-in-normal-state ()
  "Test `vimp-execute-in-normal-state'."
  :tags '(vimp)
  (ert-info ("Execute normal state command in insert state")
    (vimp-test-buffer
      "[a]bcdef\n"
      ("I")
      (should (vimp-insert-state-p))
      ("\C-ox")
      (ert-info ("Should return to insert state")
        (should (vimp-insert-state-p)))
      "[b]cdef\n"
      ("\C-oA")
      (ert-info ("Should return to insert state after insert state command")
        (should (vimp-insert-state-p)))
      ("bcdef[]\n"))))

(defun vimp-test-suppress-keymap (state)
  "Verify that `self-insert-command' is suppressed in STATE"
  (vimp-test-buffer
    ";; This buffer is for notes."
    (vimp-test-change-state state)
    ;; TODO: this should be done better
    (ert-info ("Disable the state's own keymaps so that the
suppression keymap comes first")
      (setq vimp-operator-state-minor-mode nil
            vimp-operator-state-local-minor-mode nil))
    (should (eq (key-binding "Q") #'undefined))
    (ert-info ("Don't insert text")
      ;; may or may not signal an error, depending on batch mode
      (condition-case nil
          (execute-kbd-macro "QQQ")
        (error nil))
      (should (string= (buffer-substring 1 4) ";; ")))))

(ert-deftest vimp-test-emacs-state-suppress-keymap ()
  "`self-insert-command' works in Emacs state"
  :tags '(vimp state)
  (should-error (vimp-test-suppress-keymap 'emacs)))

(ert-deftest vimp-test-normal-state-suppress-keymap ()
  "No `self-insert-command' in Normal state"
  :tags '(vimp state)
  (vimp-test-suppress-keymap 'normal))

(ert-deftest vimp-test-operator-state-suppress-keymap ()
  "Operator-Pending state should inherit suppression
of `self-insert-command' from Normal state"
  :tags '(vimp state)
  (vimp-test-suppress-keymap 'operator))

(ert-deftest vimp-test-operator-state-shortcut-keymap ()
  "Enable shortcut keymap in Operator-Pending state"
  :tags '(vimp state)
  (vimp-test-buffer
    (ert-info ("Activate `vimp-operator-shortcut-map' in \
Operator-Pending state")
      (vimp-test-change-state 'operator)
      (should (rassq vimp-operator-shortcut-map
                     (vimp-state-keymaps 'operator)))
      (should (keymapp vimp-operator-shortcut-map))
      (should vimp-operator-shortcut-mode)
      (should (memq vimp-operator-shortcut-map
                    (current-active-maps))))
    (ert-info ("Deactivate `vimp-operator-shortcut-map' \
outside Operator-Pending state")
      (vimp-test-change-state 'emacs)
      (should-not vimp-operator-shortcut-mode)
      (should-not (memq vimp-operator-shortcut-map
                        (current-active-maps))))
    (ert-info ("Reset `vimp-operator-shortcut-map' \
when entering Operator-Pending state")
      (define-key vimp-operator-shortcut-map "f" 'foo)
      (should (eq (lookup-key vimp-operator-shortcut-map "f")
                  'foo))
      (vimp-test-change-state 'operator)
      (should-not (eq (lookup-key vimp-operator-shortcut-map "f")
                      'foo)))
    (ert-info ("Reset `vimp-operator-shortcut-map' \
when exiting Operator-Pending state")
      (define-key vimp-operator-shortcut-map "b" 'bar)
      (should (eq (lookup-key vimp-operator-shortcut-map "b")
                  'bar))
      (vimp-test-change-state 'emacs)
      (should-not (eq (lookup-key vimp-operator-shortcut-map "b")
                      'bar)))))

(ert-deftest vimp-test-auxiliary-maps ()
  "Test auxiliary keymaps"
  :tags '(vimp state)
  (let ((map (make-sparse-keymap)) aux)
    (ert-info ("Create a new auxiliary keymap")
      (vimp-define-key 'normal map "f" 'foo)
      (setq aux (vimp-get-auxiliary-keymap map 'normal))
      (should (vimp-auxiliary-keymap-p aux))
      (should (eq (lookup-key aux "f") 'foo)))
    (ert-info ("Add to auxiliary keymap")
      (vimp-define-key 'normal map "b" 'bar)
      (should (eq (lookup-key aux "f") 'foo))
      (should (eq (lookup-key aux "b") 'bar)))))

;;; Type system

(ert-deftest vimp-test-exclusive-type ()
  "Expand and contract the `line' type"
  :tags '(vimp type)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Return the beginning and end unchanged \
if they are the same")
        (should (equal (vimp-normalize 1 1 'exclusive)
                       (list 1 1 'exclusive))))
      (ert-info ("expand to `inclusive' if the end position \
is at the beginning of a line")
        (should (equal (vimp-normalize (1+ first-line) second-line 'exclusive)
                       (list (1+ first-line) (1- second-line) 'inclusive
                             :expanded t))))
      (ert-info ("expand to `line' if both the beginning and end \
are at the beginning of a line")
        (should (equal (vimp-normalize first-line second-line 'exclusive)
                       (list first-line second-line 'line
                             :expanded t))))
      (ert-info ("Measure as the strict difference between the end \
and the beginning")
        (should (string= (vimp-describe 1 1 'exclusive)
                         "0 characters"))
        (should (string= (vimp-describe 1 2 'exclusive)
                         "1 character"))
        (should (string= (vimp-describe 5 2 'exclusive)
                         "3 characters"))))))

(ert-deftest vimp-test-inclusive-type ()
  "Expand and contract the `inclusive' type"
  :tags '(vimp type)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (ert-info ("Include the ending character")
      (should (equal (vimp-expand 1 1 'inclusive)
                     '(1 2 inclusive :expanded t))))
    (ert-info ("Don't mind if positions are in wrong order")
      (should (equal (vimp-expand 5 2 'inclusive)
                     '(2 6 inclusive :expanded t))))
    (ert-info ("Exclude the ending character when contracting")
      (should (equal (vimp-contract 1 2 'inclusive)
                     '(1 1 inclusive :expanded nil))))
    (ert-info ("Don't mind positions' order when contracting")
      (should (equal (vimp-contract 6 2 'inclusive)
                     '(2 5 inclusive :expanded nil))))
    (ert-info ("Measure as one more than the difference")
      (should (string= (vimp-describe 1 1 'inclusive)
                       "1 character"))
      (should (string= (vimp-describe 5 2 'inclusive)
                       "4 characters")))))

(ert-deftest vimp-test-line-type ()
  "Expand the `line' type"
  :tags '(vimp type)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Expand to the whole first line")
        (should (equal (vimp-expand first-line first-line 'line)
                       (list first-line second-line 'line :expanded t)))
        (should (string= (vimp-describe first-line first-line 'line)
                         "1 line")))
      (ert-info ("Expand to the two first lines")
        (should (equal (vimp-expand first-line second-line 'line)
                       (list first-line third-line 'line :expanded t)))
        (should (string= (vimp-describe first-line second-line 'line)
                         "2 lines"))))))

(ert-deftest vimp-test-block-type ()
  "Expand and contract the `block' type"
  :tags '(vimp type)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (let* ((first-line 1)
           (second-line (progn
                          (forward-line)
                          (point)))
           (third-line (progn
                         (forward-line)
                         (point))))
      (ert-info ("Expand to a 1x1 block")
        (should (equal (vimp-expand 1 1 'block)
                       (list 1 2 'block :expanded t)))
        (should (string= (vimp-describe 1 1 'block)
                         "1 row and 1 column")))
      (ert-info ("Expand to a 2x1 block")
        (should (equal (vimp-expand first-line second-line 'block)
                       (list first-line (1+ second-line) 'block :expanded t)))
        (should (string= (vimp-describe first-line second-line 'block)
                         "2 rows and 1 column")))
      (ert-info ("Expand to a 3x2 block")
        (should (equal (vimp-expand first-line (1+ third-line) 'block)
                       (list first-line (1+ (1+ third-line))
                             'block :expanded t)))
        (should (string= (vimp-describe first-line (1+ third-line) 'block)
                         "3 rows and 2 columns")))
      (ert-info ("Contract to a 0x0 rectangle")
        (should (equal (vimp-contract 1 2 'block)
                       (list 1 1 'block :expanded nil))))
      (ert-info ("Contract to a 2x0 rectangle")
        (should (equal (vimp-contract first-line (1+ second-line) 'block)
                       (list first-line second-line 'block :expanded nil))))
      (ert-info ("Contract to a 3x1 rectangle")
        (should (equal (vimp-contract first-line (1+ (1+ third-line)) 'block)
                       (list first-line (1+ third-line)
                             'block :expanded nil)))))))

(ert-deftest vimp-test-type-transform ()
  "Test `vimp-transform'"
  :tags '(vimp type)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (ert-info ("Return positions unchanged when passed nil \
for TYPE or TRANSFORM")
      (should (equal (vimp-transform nil 1 2 'block)
                     '(1 2 block)))
      (should (equal (vimp-transform :expand 1 2 nil)
                     '(1 2)))
      (should (equal (vimp-transform nil 1 2 nil)
                     '(1 2))))
    (ert-info ("Accept markers, but return positions")
      (should (equal (vimp-transform :expand
                                     (move-marker (make-marker) 1) 1
                                     'inclusive)
                     '(1 2 inclusive :expanded t)))
      (should (equal (vimp-transform nil (move-marker (make-marker) 1) 2
                                     nil)
                     '(1 2))))))

(ert-deftest vimp-test-type-modifiers ()
  "Test type modifiers like \"dv}\""
  :tags '(vimp type)
  (ert-info ("Change `inclusive' motions to `exclusive'")
    (vimp-test-buffer
      "[A]bove some line"
      ("dve")
      "[e] some line"))
  (ert-info ("Change `exclusive' motions to `inclusive'")
    (vimp-test-buffer
      "Above [s]ome line

Below some empty line"
      ("dv}")
      "Above[ ]
Below some empty line"))
  (ert-info ("Change type to `line'")
    (vimp-test-buffer
      "Above [s]ome line

Below some empty line"
      ("dV}")
      "[B]elow some empty line")))

;;; Insertion

(ert-deftest vimp-test-insert ()
  "Test `vimp-insert'"
  :tags '(vimp insert)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("ivimp rulz " [escape])
    ";; vimp rulz[ ]This buffer is for notes you don't want to save"))

(ert-deftest vimp-test-append ()
  "Test `vimp-append'"
  :tags '(vimp insert)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("avimp rulz " [escape])
    ";; Tvimp rulz[ ]his buffer is for notes you don't want to save"))

(ert-deftest vimp-test-open-above ()
  "Test `vimp-open-above'"
  :tags '(vimp insert)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
    ("Oabc\ndef" [escape])
    ";; This buffer is for notes you don't want to save,
abc
de[f]
;; and for Lisp evaluation.")
  (ert-info ("Open empty line")
    (vimp-test-buffer
      "(let (var)\n  [t]est)\n"
      (emacs-lisp-mode)
      ("O" [escape])
      "(let (var)\n[\n]  test)\n"))
  (ert-info ("Open non-empty line")
    (vimp-test-buffer
      "(let (var)\n  [t]est)\n"
      (emacs-lisp-mode)
      ("Odo-it" [escape])
      "(let (var)\n  do-i[t]\n  test)\n")))

(ert-deftest vimp-test-open-below ()
  "Test `vimp-open-below'"
  :tags '(vimp insert)
  (vimp-test-buffer
    "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("oabc\ndef" [escape])
    ";; This buffer is for notes you don't want to save,
abc
de[f]
;; and for Lisp evaluation.")
  (ert-info ("Open empty line")
    (vimp-test-buffer
      "[(]let (var)\n  test)\n"
      (emacs-lisp-mode)
      ("o" [escape])
      "(let (var)\n[\n]  test)\n"))
  (ert-info ("Open non-empty line")
    (vimp-test-buffer
      "[(]let (var)\n  test)\n"
      (emacs-lisp-mode)
      ("odo-it" [escape])
      "(let (var)\n  do-i[t]\n  test)\n"))
  (let ((vimp-auto-indent t))
    (ert-info ("With count")
      (vimp-test-buffer
        "[(]and a\n     c)\n"
        (emacs-lisp-mode)
        ("3ob" [escape])
        "(and a\n     b\n     b\n     [b]\n     c)\n"))))

(ert-deftest vimp-test-open-below-folded ()
  "Test `vimp-open-below' on folded lines"
  :tags '(vimp insert)
  (vimp-test-buffer
    "[l]ine1\n\n(let ()\n  var)\n\nlast line\n"
    (emacs-lisp-mode)
    (hs-minor-mode 1)
    ("zm2joABC" [escape])
    "line1\n\n(let ()\n  var)\nAB[C]\n\nlast line\n"))

(ert-deftest vimp-test-insert-line ()
  "Test `vimp-insert-line'"
  :tags '(vimp insert)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("Ivimp rulz " [escape])
    "vimp rulz[ ];; This buffer is for notes you don't want to save"))

(ert-deftest vimp-test-append-line ()
  "Test `vimp-append-line'"
  :tags '(vimp insert)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save"
    ("Avimp rulz " [escape])
    ";; This buffer is for notes you don't want to savevimp rulz[ ]"))

(ert-deftest vimp-test-insert-digraph ()
  "Test `vimp-insert-digraph'"
  :tags '(vimp insert)
  (ert-info ("Predefined digraph")
    (vimp-test-buffer
      ("i\C-kae")
      "æ[]"))
  (ert-info ("Custom digraph")
    (let ((vimp-digraphs-table-user '(((?a ?o) . ?å))))
      (vimp-test-buffer
        ("i\C-kao")
        "å[]"))))

;;; Repeat system

(ert-deftest vimp-test-normalize-repeat-info ()
  "Test `vimp-normalize-repeat-info'"
  :tags '(vimp repeat)
  (ert-info ("Single array")
    (should (equal (vimp-normalize-repeat-info
                    '("abc"))
                   '([?a ?b ?c])))
    (should (equal (vimp-normalize-repeat-info
                    '("\M-f"))
                   (list (kbd "M-f")))))
  (ert-info ("Single symbol")
    (should (equal (vimp-normalize-repeat-info
                    '(SYM))
                   '(SYM))))
  (ert-info ("Arrays only")
    (should (equal (vimp-normalize-repeat-info
                    '("abc" [XX YY] "def"))
                   '([?a ?b ?c XX YY ?d ?e ?f]))))
  (ert-info ("Several symbols")
    (should (equal (vimp-normalize-repeat-info
                    '(BEG MID END))
                   '(BEG MID END))))
  (ert-info ("Arrays with symbol at the beginning")
    (should (equal (vimp-normalize-repeat-info
                    '(BEG "abc" [XX YY] "def"))
                   '(BEG [?a ?b ?c XX YY ?d ?e ?f]))))
  (ert-info ("Arrays with symbol at the end")
    (should (equal (vimp-normalize-repeat-info
                    '("abc" [XX YY] "def" END))
                   '([?a ?b ?c XX YY ?d ?e ?f] END))))
  (ert-info ("Arrays with symbol in the middle")
    (should (equal (vimp-normalize-repeat-info
                    '("abc" [XX YY] MID "def" ))
                   '([?a ?b ?c XX YY] MID [?d ?e ?f]))))
  (ert-info ("Concatenate arrays with several symbols")
    (should (equal (vimp-normalize-repeat-info
                    '(BEG "abc" [XX YY] MID "def" END))
                   '(BEG [?a ?b ?c XX YY] MID [?d ?e ?f] END)))))

(defun vimp-test-repeat-info (keys &optional recorded)
  "Execute a sequence of keys and verify that `vimp-repeat-ring'
records them correctly. KEYS is the sequence of keys to execute.
RECORDED is the expected sequence of recorded events.
If nil, KEYS is used."
  (execute-kbd-macro keys)
  (should (equal (vimp-normalize-repeat-info (ring-ref vimp-repeat-ring 0))
                 (list (vconcat (or recorded keys))))))

(ert-deftest vimp-test-normal-repeat-info-simple-command ()
  "Save key-sequence after simple editing command in Normal state"
  :tags '(vimp repeat)
  (vimp-test-buffer
    "[T]his is a test buffer"
    (ert-info ("Call simple command without count")
      (vimp-test-repeat-info "x"))
    (ert-info ("Call simple command with count 3")
      (vimp-test-repeat-info "3x"))))

(ert-deftest vimp-test-normal-repeat-info-char-command ()
  "Save key-sequence after editing command with character in Normal state"
  :tags '(vimp repeat)
  (vimp-test-buffer
    "[T]his is a test buffer"
    (ert-info ("Call command with character argument without count")
      (vimp-test-repeat-info "r5"))
    (ert-info ("Call command with character argument with count 12")
      (vimp-test-repeat-info "12rX"))))

(ert-deftest vimp-test-insert-repeat-info ()
  "Save key-sequence after Insert state"
  :tags '(vimp repeat)
  (vimp-test-buffer
    (ert-info ("Insert text without count")
      (vimp-test-repeat-info (vconcat "iABC" [escape])))
    (ert-info ("Insert text with count 42")
      (vimp-test-repeat-info (vconcat "42iABC" [escape])))))

(ert-deftest vimp-test-repeat ()
  "Repeat several editing commands"
  :tags '(vimp repeat)
  (ert-info ("Repeat replace")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("rX")
      "[X]; This buffer is for notes you don't want to save"
      ([right right] ".")
      "X;[X]This buffer is for notes you don't want to save"))
  (ert-info ("Repeat replace with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("2rX")
      "X[X] This buffer is for notes you don't want to save"
      ([right right] ".")
      "XX X[X]is buffer is for notes you don't want to save"))
  (ert-info ("Repeat replace without count with a new count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("rX")
      "[X]; This buffer is for notes you don't want to save"
      ([right right] "13.")
      "X;XXXXXXXXXXXX[X]is for notes you don't want to save"))
  (ert-info ("Repeat replace with count replacing original count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("10rX")
      "XXXXXXXXX[X]ffer is for notes you don't want to save"
      ([right right] "20.")
      "XXXXXXXXXXfXXXXXXXXXXXXXXXXXXX[X] don't want to save"))
  (ert-info ("Repeat movement in Insert state")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save"
      ("i(\M-f)" [escape])
      ";; (This[)] buffer is for notes you don't want to save"
      ("w.")
      ";; (This) (buffer[)] is for notes you don't want to save")))

(ert-deftest vimp-test-repeat-register ()
  "Test repeating a register command."
  :tags '(vimp repeat)
  (vimp-test-buffer
    "[l]ine 1\nline 2\nline 3\nline 4\n"
    ("\"addyy\"aP")
    "[l]ine 1\nline 2\nline 3\nline 4\n"
    (".")
    "[l]ine 1\nline 1\nline 2\nline 3\nline 4\n"))

(ert-deftest vimp-test-repeat-numeric-register ()
  "Test repeating a command with a numeric register."
  :tags '(vimp repeat)
  (vimp-test-buffer
    "[l]ine 1\nline 2\nline 3\nline 4\nline 5\n"
    ("dd...")
    "[l]ine 5\n"
    ("\"1P")
    "[l]ine 4\nline 5\n"
    (".")
    "[l]ine 3\nline 4\nline 5\n"
    (".")
    "[l]ine 2\nline 3\nline 4\nline 5\n"
    (".")
    "[l]ine 1\nline 2\nline 3\nline 4\nline 5\n"))

(ert-deftest vimp-test-cmd-replace-char ()
  "Calling `vimp-replace-char' should replace characters"
  :tags '(vimp repeat)
  (vimp-test-buffer
    "[;]; This buffer is for notes you don't want to save"
    ("r5")
    "[5]; This buffer is for notes you don't want to save"
    ("3rX")
    "XX[X]This buffer is for notes you don't want to save")
  (ert-info ("Replace digraph")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save"
      ("re'")
      "[é]; This buffer is for notes you don't want to save"
      ("3rc*")
      "ξξ[ξ]This buffer is for notes you don't want to save"))
  (ert-info ("Replacing \\n should insert only one newline")
    (vimp-test-buffer
      "(setq var xxx [y]yy zzz)\n"
      (emacs-lisp-mode)
      (setq indent-tabs-mode nil)
      ("2r\n")
      "(setq var xxx \n      [y] zzz)\n")))

(ert-deftest vimp-test-insert-with-count ()
  "Test `vimp-insert' with repeat count"
  :tags '(vimp repeat)
  (vimp-test-buffer
    ";; [T]his buffer is for notes"
    ("2ivimp rulz " [escape])
    ";; vimp rulz vimp rulz[ ]This buffer is for notes"))

(ert-deftest vimp-test-repeat-insert ()
  "Test repeating of `vimp-insert'"
  :tags '(vimp repeat)
  (ert-info ("Repeat insert")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("iABC" [escape])
      "AB[C];; This buffer is for notes"
      ("..")
      "ABABAB[C]CC;; This buffer is for notes"))
  (ert-info ("Repeat insert with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("2iABC" [escape])
      "ABCAB[C];; This buffer is for notes"
      ("..")
      "ABCABABCABABCAB[C]CC;; This buffer is for notes"))
  (ert-info ("Repeat insert with repeat count")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("iABC" [escape])
      "AB[C];; This buffer is for notes"
      ("11.")
      "ABABCABCABCABCABCABCABCABCABCABCAB[C]C;; This buffer is for notes"))
  (ert-info ("Repeat insert with count with repeat with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("10iABC" [escape])
      "ABCABCABCABCABCABCABCABCABCAB[C];; This buffer is for notes"
      ("11.")
      "ABCABCABCABCABCABCABCABCABCABABCABCABCABCABCABCABCABCABCABCAB[C]C;; \
This buffer is for notes")))

(ert-deftest vimp-test-repeat-error ()
  "Test whether repeat returns to normal state in case of an error."
  (vimp-test-buffer
    "[l]ine 1\nline 2\nline 3\nline 4"
    ("ixxx" [down] [down] [left] [left] [left] "yyy" [escape])
    "xxxline 1\nline 2\nyy[y]line 3\nline 4"
    (should-error (execute-kbd-macro "j^."))
    (should (vimp-normal-state-p))
    ("^")
    "xxxline 1\nline 2\nyyyline 3\n[x]xxline 4"))

(ert-deftest vimp-test-insert-vcount ()
  "Test `vimp-insert' with vertical repeating"
  :tags '(vimp repeat)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; Below the empty line."
    (define-key vimp-normal-state-local-map "i"
      #'(lambda (count)
          (interactive "p")
          (vimp-insert count 5)))
    ("2iABC" [escape])
    "\
;; ABCAB[C]This buffer is for notes you don't want to save.
;; ABCABCIf you want to create a file, visit that file with C-x C-f,
;; ABCABCthen enter the text in that file's own buffer.
   ABCABC
;; ABCABCBelow the empty line."))

(ert-deftest vimp-test-append-with-count ()
  "Test `vimp-append' with repeat count"
  :tags '(vimp repeat)
  (vimp-test-buffer
    ";; [T]his buffer is for notes"
    ("2avimp rulz " [escape])
    ";; Tvimp rulz vimp rulz[ ]his buffer is for notes"))

(ert-deftest vimp-test-repeat-append ()
  "Test repeating of `vimp-append'"
  :tags '(vimp repeat)
  (ert-info ("Repeat insert")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("aABC" [escape])
      ";AB[C]; This buffer is for notes"
      ("..")
      ";ABCABCAB[C]; This buffer is for notes"))
  (ert-info ("Repeat insert with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("2aABC" [escape])
      ";ABCAB[C]; This buffer is for notes"
      ("..")
      ";ABCABCABCABCABCAB[C]; This buffer is for notes"))
  (ert-info ("Repeat insert with repeat count")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("aABC" [escape])
      ";AB[C]; This buffer is for notes"
      ("11.")
      ";ABCABCABCABCABCABCABCABCABCABCABCAB[C]; This buffer is for notes"))
  (ert-info ("Repeat insert with count with repeat with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes"
      ("10aABC" [escape])
      ";ABCABCABCABCABCABCABCABCABCAB[C]; This buffer is for notes"
      ("11.")
      ";ABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCAB[C]; \
This buffer is for notes")))

(ert-deftest vimp-test-append-vcount ()
  "Test `vimp-append' with vertical repeating"
  :tags '(vimp repeat)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

;; Below the empty line."
    (define-key vimp-normal-state-local-map "a"
      #'(lambda (count)
          (interactive "p")
          (vimp-append count 5)))
    ("2aABC" [escape])
    "\
;; TABCAB[C]his buffer is for notes you don't want to save.
;; IABCABCf you want to create a file, visit that file with C-x C-f,
;; tABCABChen enter the text in that file's own buffer.
    ABCABC
;; BABCABCelow the empty line."))

(ert-deftest vimp-test-open-above-with-count ()
  "Test `vimp-open-above' with repeat count"
  :tags '(vimp repeat)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
    ("2Ovimp\nrulz" [escape])
    ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation."))

(ert-deftest vimp-test-repeat-open-above ()
  "Test repeating of `vimp-open-above'"
  :tags '(vimp repeat)
  (ert-info ("Repeat insert")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save."
      ("Ovimp\nrulz" [escape])
      "vimp\nrul[z]
;; This buffer is for notes you don't want to save."
      ("..")
      "vimp\nvimp\nvimp\nrul[z]\nrulz\nrulz
;; This buffer is for notes you don't want to save."))
  (ert-info ("Repeat insert with count")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save."
      ("2Ovimp\nrulz" [escape])
      "vimp\nrulz\nvimp\nrul[z]
;; This buffer is for notes you don't want to save."
      ("..")
      "vimp\nrulz\nvimp\nvimp\nrulz\nvimp\nvimp\nrulz\nvimp\nrul[z]\nrulz\nrulz
;; This buffer is for notes you don't want to save."))
  (ert-info ("Repeat insert with repeat count")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save."
      ("Ovimp\nrulz" [escape])
      "vimp\nrul[z]\n;; This buffer is for notes you don't want to save."
      ("2.")
      "vimp\nvimp\nrulz\nvimp\nrul[z]\nrulz
;; This buffer is for notes you don't want to save."))
  (ert-info ("Repeat insert with count with repeat with count")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save."
      ("2Ovimp\nrulz" [escape])
      "vimp\nrulz\nvimp\nrul[z]
;; This buffer is for notes you don't want to save."
      ("3.")
      "vimp\nrulz\nvimp\nvimp\nrulz\nvimp\nrulz\nvimp\nrul[z]\nrulz
;; This buffer is for notes you don't want to save.")))

(ert-deftest vimp-test-open-below-with-count ()
  "Test insertion of `vimp-open-below' with repeat count"
  :tags '(vimp repeat)
  (vimp-test-buffer
    "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("2ovimp\nrulz" [escape])
    ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation."))

(ert-deftest vimp-test-repeat-open-below ()
  "Test repeating `vimp-open-below'"
  :tags '(vimp repeat)
  (ert-info ("Repeat insert")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("ovimp\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
vimp\nrul[z]\n;; and for Lisp evaluation."
      ("..")
      ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation."))
  (ert-info ("Repeat insert with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2ovimp\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation."
      ("..")
      ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrulz\nvimp\nrulz\nvimp\nrulz\nvimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation."))
  (ert-info ("Repeat insert with repeat count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("ovimp\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
vimp\nrul[z]\n;; and for Lisp evaluation."
      ("2.")
      ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation."))
  (ert-info ("Repeat insert with count with repeat with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2ovimp\nrulz" [escape])
      ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation."
      ("3.")
      ";; This buffer is for notes you don't want to save,
vimp\nrulz\nvimp\nrulz\nvimp\nrulz\nvimp\nrulz\nvimp\nrul[z]
;; and for Lisp evaluation.")))

(ert-deftest vimp-test-insert-line-with-count ()
  "Test `vimp-insert-line' with repeat count"
  :tags '(vimp repeat)
  (vimp-test-buffer
    ";; [T]his buffer is for notes"
    ("2Ivimp rulz " [escape])
    "vimp rulz vimp rulz[ ];; This buffer is for notes"))

(ert-deftest vimp-test-repeat-insert-line ()
  "Test repeating of `vimp-insert-line'"
  :tags '(vimp repeat)
  (ert-info ("Repeat insert")
    (vimp-test-buffer
      ";; This buffer is for note[s]"
      ("IABC" [escape])
      "AB[C];; This buffer is for notes"
      ("..")
      "AB[C]ABCABC;; This buffer is for notes"))
  (ert-info ("Repeat insert with count")
    (vimp-test-buffer
      ";; This buffer is for note[s]"
      ("2IABC" [escape])
      "ABCAB[C];; This buffer is for notes"
      ("..")
      "ABCAB[C]ABCABCABCABC;; This buffer is for notes"))
  (ert-info ("Repeat insert with repeat count")
    (vimp-test-buffer
      ";; This buffer is for note[s]"
      ("IABC" [escape])
      "AB[C];; This buffer is for notes"
      ("11.")
      "ABCABCABCABCABCABCABCABCABCABCAB[C]ABC;; This buffer is for notes"))
  (ert-info ("Repeat insert with count with repeat with count")
    (vimp-test-buffer
      ";; This buffer is for note[s]"
      ("10IABC" [escape])
      "ABCABCABCABCABCABCABCABCABCAB[C];; This buffer is for notes"
      ("11.")
      "ABCABCABCABCABCABCABCABCABCABCAB[C]ABCABCABCABCABCABCABCABCABCABC;; This buffer is for notes")))

(ert-deftest vimp-test-insert-line-vcount ()
  "Test `vimp-insert-line' with vertical repeating"
  :tags '(vimp repeat)
  (vimp-test-buffer
    "int[ ]main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    (define-key vimp-normal-state-local-map "I"
      #'(lambda (count)
          (interactive "p")
          (vimp-insert-line count 4)))
    ("2IABC" [escape])
    "ABCABCint main(int argc, char** argv)
ABCABC{
  ABCABCprintf(\"Hello world\\n\");
  ABCABCreturn EXIT_SUCCESS;
}"))

(ert-deftest vimp-test-append-line-with-count ()
  "Test `vimp-append-line' with repeat count"
  :tags '(vimp repeat)
  (vimp-test-buffer
    ";; [T]his buffer is for notes."
    ("2Avimp rulz " [escape])
    ";; This buffer is for notes.vimp rulz vimp rulz[ ]"))

(ert-deftest vimp-test-repeat-append-line ()
  "Test repeating of `vimp-append-line'"
  :tags '(vimp repeat)
  (ert-info ("Repeat insert")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("AABC" [escape])
      ";; This buffer is for notes.AB[C]"
      ("..")
      ";; This buffer is for notes.ABCABCAB[C]"))
  (ert-info ("Repeat insert with count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("2AABC" [escape])
      ";; This buffer is for notes.ABCAB[C]"
      ("..")
      ";; This buffer is for notes.ABCABCABCABCABCAB[C]"))
  (ert-info ("Repeat insert with repeat count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("AABC" [escape])
      ";; This buffer is for notes.ABC"
      ("11.")
      ";; This buffer is for notes.ABCABCABCABCABCABCABCABCABCABCABCAB[C]"))
  (ert-info ("Repeat insert with count with repeat with count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("10AABC" [escape])
      ";; This buffer is for notes.ABCABCABCABCABCABCABCABCABCAB[C]"
      ("11.")
      ";; This buffer is for notes.ABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCABCAB[C]")))

(ert-deftest vimp-test-append-line-vcount ()
  "Test `vimp-append-line' with vertical repeating"
  :tags '(vimp repeat)
  (vimp-test-buffer
    "int[ ]main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    (define-key vimp-normal-state-local-map "A"
      #'(lambda (count)
          (interactive "p")
          (vimp-append-line count 4)))
    ("2AABC" [escape])
    "int main(int argc, char** argv)ABCAB[C]
{ABCABC
  printf(\"Hello world\\n\");ABCABC
  return EXIT_SUCCESS;ABCABC
}"))

(ert-deftest vimp-test-repeat-by-change ()
  "Test repeating by tracking changes for completion commands"
  :tags '(vimp repeat)
  (let ((line-move-visual nil)
        (change (vimp-define-command nil ()
                  :repeat change
                  (interactive)
                  (delete-char 5)
                  (insert "BEGIN\n")
                  (save-excursion
                    (insert "\nEND\n")))))
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      (define-key vimp-insert-state-local-map (kbd "C-c C-p") change)
      ("iABC " (kbd "C-c C-p") "BODY" [escape])
      ";; ABC BEGIN
BOD[Y]
END
buffer is for notes."
      (".")
      ";; ABC BEGIN
BODABC BEGIN
BOD[Y]
END

buffer is for notes.")))

(ert-deftest vimp-test-repeat-kill-buffer ()
  "Test safe-guard preventing buffers from being deleted
when repeating a command"
  :tags '(vimp repeat)
  (ert-info ("Test killing works for direct calls \
to `vimp-execute-repeat-info'")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      (setq vimp-repeat-ring (make-ring 10))
      (ring-insert vimp-repeat-ring '((kill-buffer nil)))
      (vimp-execute-repeat-info (ring-ref vimp-repeat-ring 0))
      (should-not (looking-at ";; This"))))
  (ert-info ("Verify an error is raised when using \
the `vimp-repeat' command")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      (setq vimp-repeat-ring (make-ring 10))
      (ring-insert vimp-repeat-ring '((kill-buffer nil)))
      (vimp-execute-repeat-info (ring-ref vimp-repeat-ring 0))
      (should-error (call-interactively #'vimp-repeat)))))

(ert-deftest vimp-test-repeat-pop ()
  "Test `repeat-pop'."
  :tags '(vimp repeat)
  (ert-info ("Test repeat-pop")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      (setq vimp-repeat-ring (make-ring 10))
      ("iABC" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      (".")
      ";; ABCXYZXY[Z]This buffer is for notes."))
  (ert-info ("Test repeat-pop")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      (setq vimp-repeat-ring (make-ring 10))
      ("iABC" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      ("." (kbd "C-."))
      ";; ABCXYAB[C]ZThis buffer is for notes."))
  (ert-info ("Test repeat-pop-next")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      (setq vimp-repeat-ring (make-ring 10))
      ("iABC" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      ("." (kbd "C-.") (kbd "M-."))
      ";; ABCXYZXY[Z]This buffer is for notes."))
  (ert-info ("Test repeat-pop after non-change")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      (setq vimp-repeat-ring (make-ring 10))
      ("iABC" [escape] "a" [escape] "aXYZ" [escape])
      ";; ABCXY[Z]This buffer is for notes."
      ("." (kbd "C-.") (kbd "C-."))
      ";; ABCXYAB[C]ZThis buffer is for notes.")))

(ert-deftest vimp-test-ESC-repeat-normal-state ()
  "Test if ESC is not been recorded in normal state."
  :tags '(vimp repeat)
  (ert-info ("Test normal ESC")
    (vimp-test-buffer
      ";;[ ]This buffer is for notes."
      (setq vimp-repeat-ring (make-ring 10))
      (should (= (ring-length vimp-repeat-ring) 0))
      ("aABC" [escape])
      ";; AB[C]This buffer is for notes."
      (should (= (ring-length vimp-repeat-ring) 1))
      (".")
      ";; ABCAB[C]This buffer is for notes."
      ([escape])
      (should (= (ring-length vimp-repeat-ring) 1))
      (".")
      ";; ABCABCAB[C]This buffer is for notes.")))

(ert-deftest vimp-test-abort-operator-repeat ()
  "Test if ESC in operator-state cancels recording of repeation."
  :tags '(vimp repeat)
  (let ((inhibit-quit t))
    (ert-info ("Test ESC")
      (vimp-test-buffer
        ";;[ ]This buffer is for notes."
        (setq vimp-repeat-ring (make-ring 10))
        (should (= (ring-length vimp-repeat-ring) 0))
        ("aABC" [escape])
        ";; AB[C]This buffer is for notes."
        (should (= (ring-length vimp-repeat-ring) 1))
        (".")
        ";; ABCAB[C]This buffer is for notes."
        ("d" [escape])
        (should (= (ring-length vimp-repeat-ring) 1))
        (".")
        ";; ABCABCAB[C]This buffer is for notes."))))

(ert-deftest vimp-test-repeat-visual-char ()
  "Test repeat of character visual mode command."
  :tags '(vimp repeat)
  (ert-info ("Test repeat on same line")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("v3lcABC" [escape])
      ";; AB[C] buffer is for notes."
      ("ww.")
      ";; ABC buffer AB[C]or notes."))
  (ert-info ("Test repeat on several lines")
    (vimp-test-buffer
      ";; This [b]uffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("vj^eerX")
      ";; This XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXX[X] you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("2gg^3w.")
      ";; This XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXXX you want XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
XXXX[X]en enter the text in that file's own buffer.
")))

(ert-deftest vimp-test-repeat-visual-line ()
  "Test repeat of linewise visual mode command."
  :tags '(vimp repeat)
  (ert-info ("Test repeat on several lines")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter th[e] text in that file's own buffer.

;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("VkcNew Text" [escape])
      ";; This buffer is for notes you don't want to save.
New Tex[t]

;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("jj.")
      ";; This buffer is for notes you don't want to save.
New Text

New Tex[t]
;; then enter the text in that file's own buffer.
")))

(ert-deftest vimp-test-repeat-visual-block ()
  "Test repeat of block visual mode command."
  :tags '(vimp repeat)
  (ert-info ("Test repeat on several lines")
    (vimp-test-buffer
      ";; This [b]uffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ((kbd "C-v") "3j2lrQ")
      ";; This [Q]QQfer is for notes you don't want to save.
;; If yoQQQant to create a file, visit that file with C-x C-f,
;; then QQQer the text in that file's own buffer.
;; This QQQfer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
"
      ("2j3w.")
      ";; This QQQfer is for notes you don't want to save.
;; If yoQQQant to create a file, visit that file with C-x C-f,
;; then QQQer the text [Q]QQthat file's own buffer.
;; This QQQfer is for nQQQs you don't want to save.
;; If you want to creatQQQ file, visit that file with C-x C-f,
;; then enter the text QQQthat file's own buffer.
")))

(ert-deftest vimp-visual-block-append ()
  "Test appending in visual block."
  :tags '(vimp visual insert)
  (ert-info ("Simple append")
    (vimp-test-buffer
      "l[i]ne 1\nline 2\nline 3\n"
      ((kbd "C-v") "jjllAXXX" [escape])
      "lineXX[X] 1\nlineXXX 2\nlineXXX 3\n"))
  (ert-info ("Append after empty lines")
    (vimp-test-buffer
      "line 1l[i]ne 1\nline 2\nline 3line 3\n"
      (setq indent-tabs-mode nil)
      ((kbd "C-v") "jjllAXXX" [escape])
      "line 1lineXX[X] 1\nline 2    XXX\nline 3lineXXX 3\n"))
  (ert-info ("Append after empty first line")
    (vimp-test-buffer
      "l[i]ne 1line 1\nline 2\nline 3line 3line 3\n"
      (setq indent-tabs-mode nil)
      ((kbd "C-v") "jj3feAXXX" [escape])
      "line 1line 1    XX[X]\nline 2          XXX\nline 3line 3lineXXX 3\n"))
  (ert-info ("Append after end of lines")
    (vimp-test-buffer
      "line 1l[i]ne 1line 1\nline 2\nline 3line 3\n"
      (setq indent-tabs-mode nil)
      ((kbd "C-v") "jj$AXXX" [escape])
      "line 1line 1line 1XX[X]\nline 2XXX\nline 3line 3XXX\n")))

(ert-deftest vimp-test-repeat-digraph ()
  "Test repeat of insertion of a digraph."
  :tags '(vimp digraph repeat)
  (vimp-test-buffer
    "Line with ['] several apostrophes ', yeah."
    ("s" (kbd "C-k") "'9" [escape])
    "Line with [’] several apostrophes ', yeah."
    ("f'.")
    "Line with ’ several apostrophes [’], yeah."))

;;; Operators

(ert-deftest vimp-test-keypress-parser ()
  "Test `vimp-keypress-parser'"
  :tags '(vimp operator)
  (vimp-test-buffer
    :state operator
    (ert-info ("Read from the keyboard unless INPUT is given")
      (vimp-test-buffer
        :state operator
        (let ((unread-command-events '(?d)))
          (should (equal (vimp-keypress-parser)
                         '(vimp-delete nil)))
          (should (equal (vimp-keypress-parser '(?d))
                         '(vimp-delete nil))))))
    (ert-info ("Read remainder from the keyboard if INPUT is incomplete")
      (let ((unread-command-events '(?d)))
        (should (equal (vimp-keypress-parser '(?2))
                       '(vimp-delete 2)))))
    (ert-info ("Handle counts not starting with zero")
      (should (equal (vimp-keypress-parser '(?2 ?d))
                     '(vimp-delete 2)))
      (should (equal (vimp-keypress-parser '(?2 ?0 ?d))
                     '(vimp-delete 20)))
      (should (equal (vimp-keypress-parser '(?2 ?0 ?2 ?d))
                     '(vimp-delete 202)))
      (should (equal (vimp-keypress-parser '(?4 ?0 ?4 ?g ??))
                     '(vimp-rot13 404))))
    (ert-info ("Treat 0 as a motion")
      (should (equal
               (vimp-keypress-parser '(?0))
               '(vimp-digit-argument-or-vimp-beginning-of-line nil))))
    (ert-info ("Handle keyboard macros")
      (vimp-test-buffer
        (define-key vimp-motion-state-local-map (kbd "W") (kbd "w"))
        (should (equal (vimp-keypress-parser '(?W))
                       '(vimp-forward-word-begin nil)))))))

(ert-deftest vimp-test-invert-char ()
  "Test `vimp-invert-char'"
  :tags '(vimp operator)
  (vimp-test-buffer
    ";; [T]his buffer is for notes."
    ("~")
    ";; t[h]is buffer is for notes.")
  (vimp-test-buffer
    ";; <[T]his> buffer is for notes."
    ("~")
    ";; [t]HIS buffer is for notes.")
  (vimp-test-buffer
    :visual block
    ";; <[T]his buffer is for notes,
;; and >for Lisp evaluation."
    ("~")
    ";; [t]HIS buffer is for notes,
;; AND for Lisp evaluation."))

(ert-deftest vimp-test-rot13 ()
  "Test `vimp-rot13'"
  :tags '(vimp operator)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save."
    ("g?" [M-right])
    ";; [G]uvf buffer is for notes you don't want to save."))

(ert-deftest vimp-test-rot13-with-count ()
  "Test `vimp-rot13' with count argument"
  :tags '(vimp operator)
  (ert-info ("Count before operator")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("2g?" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."))
  (ert-info ("Count before motion")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("g?2" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."))
  (ert-info ("Count before operator and motion")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("3g?2" [M-right])
      ";; [G]uvf ohssre vf sbe abgrf lbh don't want to save."))
  (ert-info ("Count exceeding buffer boundaries")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("g?200" [right])
      ";; [G]uvf ohssre vf sbe abgrf lbh qba'g jnag gb fnir.")))

(ert-deftest vimp-test-rot13-repeat ()
  "Test repeating of `vimp-rot13'"
  :tags '(vimp operator)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save."
    ("g?" [M-right] [M-right])
    ";; Guvf[ ]buffer is for notes you don't want to save."
    (".")
    ";; Guvf[ ]ohssre is for notes you don't want to save."))

(ert-deftest vimp-test-rot13-repeat-with-count ()
  "Test repeating of `vimp-rot13' with new count"
  :tags '(vimp operator)
  (ert-info ("Count before operator")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("2g?" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."
      ("3.")
      ";; [T]his buffer vf for notes you don't want to save."))
  (ert-info ("Count before motion")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("g?2" [M-right])
      ";; [G]uvf ohssre is for notes you don't want to save."
      ("3.")
      ";; [T]his buffer vf for notes you don't want to save."))
  (ert-info ("Count before operator and motion")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("3g?2" [M-right])
      ";; [G]uvf ohssre vf sbe abgrf lbh don't want to save."
      ("4.")
      ";; [T]his buffer is for abgrf lbh don't want to save.")))

(ert-deftest vimp-test-operator-delete ()
  "Test deleting text"
  :tags '(vimp operator)
  (ert-info ("Delete characters")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("dl")
      ";; [h]is buffer is for notes."
      ("d1l")
      ";; [i]s buffer is for notes."
      ("1dl")
      ";; [s] buffer is for notes."
      ("1d1l")
      ";; [ ]buffer is for notes."
      ("d2l")
      ";; [u]ffer is for notes."
      ("2dl")
      ";; [f]er is for notes."
      ("d4l")
      ";; [i]s for notes."
      ("4dl")
      ";; [o]r notes."
      ("2d2l")
      ";; [o]tes."))
  (ert-info ("Delete current line")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("dd")
      "[;]; and for Lisp evaluation.")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("d1d")
      "[;]; and for Lisp evaluation.")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("1dd")
      "[;]; and for Lisp evaluation.")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("1d1d")
      "[;]; and for Lisp evaluation."))
  (ert-info ("Delete two lines")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("d2d")
      "[;]; then enter the text in that file's own buffer.")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2dd")
      "[;]; then enter the text in that file's own buffer.")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("dj")
      "[;]; then enter the text in that file's own buffer.")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("dk")
      "[;]; then enter the text in that file's own buffer.")))

(vimp-define-motion vimp-test-square-motion (count)
  "Test motion for selecting a square."
  :type block
  (let ((column (current-column)))
    (forward-line (1- count))
    (move-to-column (+ column count -1))))

(ert-deftest vimp-test-yank ()
  "Test `vimp-yank'"
  :tags '(vimp operator)
  (ert-info ("Yank characters")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("y2e")
      (should (string= (current-kill 0) "This buffer"))))
  (ert-info ("Yank lines")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("yj")
      (should (string= (current-kill 0)
                       (buffer-substring (point-min)
                                         (1+ (line-end-position 2)))))
      (should (eq (car-safe (get-text-property 0 'yank-handler
                                               (current-kill 0)))
                  'vimp-yank-line-handler)))
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("y5j")
      (should
       (string= (current-kill 0)
                (concat (buffer-substring (line-beginning-position 1)
                                          (point-max))
                        "\n")))
      (should (eq (car-safe (get-text-property 0 'yank-handler
                                               (current-kill 0)))
                  'vimp-yank-line-handler))))
  (ert-info ("Yank rectangle")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y3s")
      (should (string= (current-kill 0) "Thi\nIf \nthe"))
      (should (eq (car-safe (get-text-property 0 'yank-handler
                                               (current-kill 0)))
                  'vimp-yank-block-handler)))))

(ert-deftest vimp-test-delete ()
  "Test `vimp-delete'"
  :tags '(vimp operator)
  (ert-info ("Delete characters")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save[.]"
      ("x")
      ";; This buffer is for notes you don't want to sav[e]"
      (goto-char 4)
      ";; [T]his buffer is for notes you don't want to save"
      ("d2e")
      ";; [ ]is for notes you don't want to save"
      (should (string= (current-kill 0) "This buffer"))
      ("P")
      ";; This buffe[r] is for notes you don't want to save"))
  (ert-info ("Delete lines")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2dd")
      "[;]; then enter the text in that file's own buffer."
      ("P")
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Delete last line")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2dd")
      "[;]; This buffer is for notes you don't want to save."))
  (ert-info ("Delete last empty line")
    (vimp-test-buffer
      "line 1\nline 2\n\n[]"
      ("dd")
      "line 1\nline 2\n[]"))
  (ert-info ("Delete rectangle")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("d3s")
      "[T]his buffer is for notes you don't want to save.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.")))

(ert-deftest vimp-test-delete-line ()
  "Test `vimp-delete-line'"
  :tags '(vimp operator)
  (ert-info ("Delete to end of line")
    (vimp-test-buffer
      ";; This buffer is for notes[ ]you don't want to save."
      ("D")
      ";; This buffer is for note[s]"))
  (ert-info ("Act linewise on character selection")
    (vimp-test-buffer
      ";; This <buffe[r]> is for notes,
and for Lisp evaluation."
      ("D")
      "[a]nd for Lisp evaluation."))
  (ert-info ("Act on each line of block selection")
    (vimp-test-buffer
      :visual block
      ";; This buffer is for <notes,
;; and for Lisp evaluatio[n]>."
      ("D")
      ";; This buffer is for[ ]
;; and for Lisp evalua"))
  (ert-info ("Yank full block with block selection")
    (vimp-test-buffer
      :visual block
      "line1 l<ine1 line1 line1\nline2 line2\nline3 lin>e3 line3\n"
      ("D")
      "line1 [l]\nline2 l\nline3 l\n"
      ("0P")
      "ine1 line1 line1line1 l
ine2            line2 l
ine3 line3      line3 l\n")))

(ert-deftest vimp-test-delete-folded ()
  "Test `vimp-delete' on folded lines."
  :tags '(vimp operator)
  (ert-info ("Delete folded lines")
    (vimp-test-buffer
      "[l]ine1\n\n(let ()\n  var)\n\n(let ()\n  var2)\n"
      (emacs-lisp-mode)
      (hs-minor-mode 1)
      ("zm2jdd")
      "line1\n\n[\n](let ()\n  var2)\n"))
  (ert-info ("Delete folded lines with count")
    (vimp-test-buffer
      "[l]ine1\n\n(let ()\n  var)\n\n(let ()\n  var2)\n\nlast line\n"
      (emacs-lisp-mode)
      (hs-minor-mode 1)
      ("zm2j3dd")
      "line1\n\n[\n]last line\n")))

(ert-deftest vimp-test-delete-backward-word ()
  "Test `vimp-delete-backward-word' in insert state."
  :tags '(vimp)
  (let ((vimp-backspace-join-lines t))
    (vimp-test-buffer
      "abc def\n   ghi j[k]l\n"
      ("i" (kbd "C-w"))
      "abc def\n   ghi [k]l\n"
      ((kbd "C-w"))
      "abc def\n   [k]l\n"
      ((kbd "C-w"))
      "abc def\n[k]l\n"
      ((kbd "C-w"))
      "abc def[k]l\n"))
  (let (vimp-backspace-join-lines)
    (vimp-test-buffer
      "abc def\n[k]l\n"
      (should-error (execute-kbd-macro (concat "i" (kbd "C-w"))))
      "abc def\n[k]l\n")))

(ert-deftest vimp-test-change ()
  "Test `vimp-change'"
  :tags '(vimp operator)
  (ert-info ("Change characters")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("c2eABC" [escape])
      ";; AB[C] is for notes you don't want to save."
      (should (string= (current-kill 0) "This buffer"))
      ("p")
      ";; ABCThis buffe[r] is for notes you don't want to save."))
  (ert-info ("Change lines")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2ccABCLINE\nDEFLINE" [escape])
      "ABCLINE
DEFLIN[E]
;; then enter the text in that file's own buffer."
      ("p")
      "ABCLINE
DEFLINE
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Change last line")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2ccABC" [escape])
      ";; This buffer is for notes you don't want to save.
AB[C]"))
  (ert-info ("Change rectangle")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("c3sABC" [escape])
      "AB[C]This buffer is for notes you don't want to save.
ABCIf you want to create a file, visit that file with C-x C-f,
ABCthen enter the text in that file's own buffer.")))

(ert-deftest vimp-test-change-word ()
  "Test changing words"
  :tags '(vimp operator)
  (ert-info ("Non-word")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("cwABC" [escape])
      "AB[C] This buffer is for notes."))
  (ert-info ("Word")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("cwABC" [escape])
      ";; AB[C] buffer is for notes."))
  (ert-info ("Single character")
    (vimp-test-buffer
      "[;] This buffer is for notes."
      ("cwABC" [escape])
      "AB[C] This buffer is for notes."))
  (ert-info ("Whitespace")
    (vimp-test-buffer
      "This[ ]is a test\n"
      ("cwABC" [escape])
      "ThisAB[C]is a test\n")))

(ert-deftest vimp-test-join ()
  "Test `vimp-join'"
  :tags '(vimp operator)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f."
      ("J")
      ";; This buffer is for notes you don't want to save.[ ]\
;; If you want to create a file, visit that file with C-x C-f."))
  (ert-info ("Visual")
    (vimp-test-buffer
      :visual line
      "<;; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f.>"
      ("J")
      ";; This buffer is for notes you don't want to save.[ ]\
;; If you want to create a file, visit that file with C-x C-f.")))

(ert-deftest vimp-test-substitute ()
  "Test `vimp-substitute'"
  :tags '(vimp operator)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("5sABC" [escape])
      ";; AB[C]buffer is for notes."))
  (ert-info ("On empty line")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      ("5sABC" [escape])
      "Above some line
AB[C]
Below some empty line")))

(ert-deftest vimp-test-shift ()
  "Test `vimp-shift-right' and `vimp-shift-left'."
  :tags '(vimp operator)
  (let ((vimp-shift-width 4)
        indent-tabs-mode)
    (ert-info ("Shift linewise")
      (vimp-test-buffer
        "[l]ine 1\nline 2\nline 3\n"
        ("Vj>")
        "    [l]ine 1\n    line 2\nline 3\n"))
    (ert-info ("Shift char selection on whole line")
      (vimp-test-buffer
        "[l]ine 1\nline 2\nline 3\n"
        ("v$>")
        "    [l]ine 1\nline 2\nline 3\n"))
    (ert-info ("Shift visual with count")
      (vimp-test-buffer
        "[l]ine 1\nline 2\nline 3\n"
        ("Vj3>")
        "            [l]ine 1\n            line 2\nline 3\n"
        ("Vj2<")
        "    [l]ine 1\n    line 2\nline 3\n"))
    (ert-info ("Shift in insert state")
      (vimp-test-buffer
        "line 1\nl[i]ne 2\nline 3\n"
        ("i\C-t\C-t")
        "line 1\n        l[i]ne 2\nline 3\n"
        ("\C-d")
        "line 1\n    l[i]ne 2\nline 3\n"))))

;;; Paste

(ert-deftest vimp-test-paste-before ()
  "Test `vimp-paste-before'"
  :tags '(vimp paste)
  (ert-info ("Paste characters")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("P")
      ";; This buffer is for notes you don't want to save,
This buffe[r];; and for Lisp evaluation."))
  (ert-info ("Paste characters with count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("3P")
      ";; This buffer is for notes you don't want to save,
This bufferThis bufferThis buffe[r];; and for Lisp evaluation."))
  (ert-info ("Paste characters at end-of-buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2eG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
      ("2P")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluationThis bufferThis buffe[r]."))
  (ert-info ("Paste lines")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yyP")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("Paste lines with count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yy2P")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("Paste lines at end-of-buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yyG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
      ("2P")
      ";; This buffer is for notes you don't want to save,
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; and for Lisp evaluation."))
  (ert-info ("Paste block")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ysP")
      "[;]; ;; This buffer is for notes you don't want to save.
;; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ys2P")
      "[;]; ;; ;; This buffer is for notes you don't want to save.
;; ;; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; ;; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with empty line")
    (vimp-test-buffer
      "[;]; Above some line

;; Below some empty line"
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ys2P")
      "[;]; ;; ;; Above some line
      \n\
;; ;; ;; Below some empty line"))
  (ert-info ("Paste block crossing end of buffer")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ysj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("P")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;"))
  (ert-info ("Paste block at end-of-line")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ys$")
      ";; This buffer is for notes you don't want to save[.]
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.[;];
;; If you want to create a file, visit that file wi;; th C-x C-f,
;; then enter the text in that file's own buffer.  ;;")))

(ert-deftest vimp-test-paste-after ()
  "Test `vimp-paste-after'"
  :tags '(vimp paste)
  (ert-info ("Paste characters")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("p")
      ";; This buffer is for notes you don't want to save,
;This buffe[r]; and for Lisp evaluation."))
  (ert-info ("Paste characters with count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2ej0")
      ";; This buffer is for notes you don't want to save,
\[;]; and for Lisp evaluation."
      ("3p")
      ";; This buffer is for notes you don't want to save,
;This bufferThis bufferThis buffe[r]; and for Lisp evaluation."))
  (ert-info ("Paste characters at end-of-buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("y2eG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
      ("2p")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.This bufferThis buffe[r]"))
  (ert-info ("Paste lines")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yyp")
      ";; This buffer is for notes you don't want to save,
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; and for Lisp evaluation."))
  (ert-info ("Paste lines with count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yy2p")
      ";; This buffer is for notes you don't want to save,
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; and for Lisp evaluation."))
  (ert-info ("Paste lines at end-of-buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("2yyG$")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation[.]"
      ("2p")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
\[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("Paste block")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ysp")
      ";[;]; ; This buffer is for notes you don't want to save.
;;; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ys2p")
      ";[;]; ;; ; This buffer is for notes you don't want to save.
;;; ;; ; If you want to create a file, visit that file with C-x C-f,
;;; ;; ; then enter the text in that file's own buffer."))
  (ert-info ("Paste block with empty line")
    (vimp-test-buffer
      "[;]; Above some line

;; Below some empty line"
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ys2p")
      ";;; ;; ; Above some line

;;; ;; ; Below some empty line"))
  (ert-info ("Paste block crossing end of buffer")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ysj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.
;;; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer.
 ;;"))
  (ert-info ("Paste block at end-of-line")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("3ys$")
      ";; This buffer is for notes you don't want to save[.]
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.;;
;; If you want to create a file, visit that file wi;; th C-x C-f,
;; then enter the text in that file's own buffer.  ;;")))

(ert-deftest vimp-test-paste-pop-before ()
  "Test `vimp-paste-pop' after `vimp-paste-before'"
  :tags '(vimp paste)
  (ert-info ("Paste")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("P")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;"))
  (ert-info ("Single pop")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjP\C-p")
      ";; This buffer is for notes you don't want to save.
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Two pops")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjP\C-p\C-p")
      ";; This buffer is for notes you don't want to save.
;; Thi[s];; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjP2\C-p")
      ";; This buffer is for notes you don't want to save.
;; Thi[s];; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Single pop-next")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjP2\C-p\C-n")
      ";; This buffer is for notes you don't want to save.
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop-next with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjP\C-p\C-p2\C-n")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;")))

(ert-deftest vimp-test-paste-pop-after ()
  "Test `vimp-paste-pop' after `vimp-paste-after'"
  :tags '(vimp paste)
  (ert-info ("Paste")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sj")
      ";; This buffer is for notes you don't want to save.
\[;]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("p")
      ";; This buffer is for notes you don't want to save.
;[;]; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer.
 ;;"))
  (ert-info ("Single pop")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjp\C-p")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Two pops")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjp\C-p\C-p")
      ";; This buffer is for notes you don't want to save.
;;; Thi[s]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjp2\C-p")
      ";; This buffer is for notes you don't want to save.
;;; Thi[s]; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Single pop-next")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjp2\C-p\C-n")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
\[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Pop-next with count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjp\C-p\C-p2\C-n")
      ";; This buffer is for notes you don't want to save.
;[;]; ; If you want to create a file, visit that file with C-x C-f,
;;; ; then enter the text in that file's own buffer.
 ;;")))

(ert-deftest vimp-test-paste-pop-without-undo ()
  "Test `vimp-paste-pop' with undo disabled"
  :tags '(vimp paste)
  (ert-info ("Pop-next with count without undo")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      (setq buffer-undo-list t)
      (define-key vimp-operator-state-local-map "s" 'vimp-test-square-motion)
      ("y2e2yyy3sjP\C-p\C-p2\C-n")
      ";; This buffer is for notes you don't want to save.
\[;]; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.
;;")))

(ert-deftest vimp-test-visual-paste ()
  "Test `vimp-paste-before' and `vimp-paste-after' in Visual state"
  :tags '(vimp paste)
  (vimp-test-buffer
    ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f."
    ("yyk")
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f."
    ("VP")
    "[;]; If you want to create a file, visit that file with C-x C-f.
;; If you want to create a file, visit that file with C-x C-f.")
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f."
    ("yyj")
    ";; This buffer is for notes you don't want to save.
;; [I]f you want to create a file, visit that file with C-x C-f."
    ("Vp")
    ";; This buffer is for notes you don't want to save.
\[;]; This buffer is for notes you don't want to save."))

(ert-deftest vimp-test-visual-paste-pop ()
  "Test `vimp-paste-pop' after visual paste."
  :tags '(vimp paste)
  (ert-info ("Visual-char paste, char paste")
    (vimp-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^jw")
      "word1a word1b word1c\nword2a [w]ord2b\nword3a word3b word3c word3d\n"
      ("viwp")
      "word1a word1b word1c\nword2a word1[b]\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-char paste, char paste, line pop")
    (vimp-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^jw")
      "word1a word1b word1c\nword2a [w]ord2b\nword3a word3b word3c word3d\n"
      ("viwp\C-p")
      "word1a word1b word1c\nword2a \n[w]ord1a word1b word1c\n\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-char paste, char paste, line pop, char pop")
    (vimp-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^jw")
      "word1a word1b word1c\nword2a [w]ord2b\nword3a word3b word3c word3d\n"
      ("viwp\C-p\C-p")
      "word1a word1b word1c\nword2a word1[a]\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-line paste, char paste")
    (vimp-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^j")
      "word1a word1b word1c\n[w]ord2a word2b\nword3a word3b word3c word3d\n"
      ("Vp")
      "word1a word1b word1c\nword1[b]word3a word3b word3c word3d\n"))
  (ert-info ("Visual-line paste, char paste, line pop")
    (vimp-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^j")
      "word1a word1b word1c\n[w]ord2a word2b\nword3a word3b word3c word3d\n"
      ("Vp\C-p")
      "word1a word1b word1c\n[w]ord1a word1b word1c\nword3a word3b word3c word3d\n"))
  (ert-info ("Visual-line paste, char paste, line pop, char pop")
    (vimp-test-buffer
      "[w]ord1a word1b word1c\nword2a word2b\nword3a word3b word3c word3d\n"
      ("yiwyywyiw^j")
      "word1a word1b word1c\n[w]ord2a word2b\nword3a word3b word3c word3d\n"
      ("Vp\C-p\C-p")
      "word1a word1b word1c\nword1[a]word3a word3b word3c word3d\n")))

(ert-deftest vimp-test-register ()
  "Test yanking and pasting to and from register."
  :tags '(vimp yank paste)
  (ert-info ("simple lower case register")
    (vimp-test-buffer
      "[f]oo\n"
      ("\"ayw\"aP")
      "fo[o]foo\n"
      ("\"ayy\"aP")
      "[f]oofoo\nfoofoo\n"))
  (ert-info ("upper case register")
    (vimp-test-buffer
      "[f]oo\n"
      ("\"ayw\"Ayw\"aP")
      "foofo[o]foo\n"
      ("\"ayy\"Ayy\"aP")
      "[f]oofoofoo\nfoofoofoo\nfoofoofoo\n"))
  (ert-info ("upper case register and lines")
    (vimp-test-buffer
      "[l]ine 1\nline 2\nline 3\nline 4\n"
      ("\"a2Yjj\"A2Y\"aP")
      "line 1\nline 2\n[l]ine 1\nline 2\nline 3\nline 4\nline 3\nline 4\n"
      ("8G\"ap")
      "line 1\nline 2\nline 1\nline 2\nline 3\nline 4\nline 3\nline 4\n[l]ine 1\nline 2\nline 3\nline 4\n"))
  (ert-info ("yank with count")
    (vimp-test-buffer
      "[l]ine 1\nline 2\nline 3\n"
      ("\"a2yw\"aP")
      "line [1]line 1\nline 2\nline 3\n"
      ("\"a2yy\"aP")
      "[l]ine 1line 1\nline 2\nline 1line 1\nline 2\nline 3\n"))
  (dolist (module '(vimp-search isearch))
    (vimp-select-search-module 'vimp-search-module module)
    (ert-info ((format "special register / (module: %s)" module))
      (vimp-test-buffer
        "[f]oo bar\n"
        ("/bar" [return] "0i\C-r/")
        "bar[f]oo bar\n")))
  (ert-info ("special register :")
    (vimp-test-buffer
      "[f]oo bar\n"
      (":noh\ni\C-r:"))))

(ert-deftest vimp-test-last-insert-register ()
  "Test last insertion register."
  (vimp-test-buffer
    "[l]ine 1\n"
    ("GiABC" [escape])
    "line 1\nAB[C]"
    ("gg\".P")
    "AB[C]line 1\nABC"))

(ert-deftest vimp-test-zero-register ()
  "\"0 contains the last text that was yanked without specificying a register."
  (vimp-test-buffer
    "[l]ine 1\nline 2\n"
    ("yy\"0p")
    "line 1\n[l]ine 1\nline 2\n"
    ("j\"ayy\"0p")
    "line 1\nline 1\nline 2\n[l]ine 1\n" ; yanked line 2 to "a, so "0 is still line 1
    ("kdd\"0p")
    "line 1\nline 1\nline 1\n[l]ine 1\n"))

(ert-deftest vimp-test-align ()
  "Test `vimp-align-left', `vimp-align-right' and `vimp-align-center'."
  :tags '(vimp operator)
  (vimp-without-display
    (let ((fill-column 70)
          indent-tabs-mode)
      (vimp-test-buffer
        "before\n[l]ine 1\nthis is line number 2\nline number 3\nafter\n"
        (":.,+2ri" [return] (kbd "M-x") "untabify" [return])
        "before\n                                                                [l]ine 1\n                                                 this is line number 2\n                                                         line number 3\nafter\n"
        (":.,+2ri 60" [return] (kbd "M-x") "untabify" [return])
        "before\n                                                      [l]ine 1\n                                       this is line number 2\n                                               line number 3\nafter\n"
        (":.,+2le" [return] (kbd "M-x") "untabify" [return])
        "before\n[l]ine 1\nthis is line number 2\nline number 3\nafter\n"
        (":.,+2le 10" [return])
        "before\n          [l]ine 1\n          this is line number 2\n          line number 3\nafter\n"
        (":.,+2ce" [return] (kbd "M-x") "untabify" [return])
        "before\n                                [l]ine 1\n                        this is line number 2\n                            line number 3\nafter\n"
        (":.,+2ce 40" [return] (kbd "M-x") "untabify" [return])
        "before\n                 [l]ine 1\n         this is line number 2\n             line number 3\nafter\n"))))

;;; Motions

(ert-deftest vimp-test-forward-char ()
  "Test `vimp-forward-char' motion"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("l")
      ";[;] This buffer is for notes."))
  (ert-info ("End of line")
    (let ((vimp-cross-lines t)
          (vimp-move-cursor-back t))
      (vimp-test-buffer
        ";; This buffer is for notes[,]
;; and for Lisp evaluation."
        ("l")
        ";; This buffer is for notes,
\[;]; and for Lisp evaluation.")))
  (ert-info ("With count")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("12l")
      ";; This buff[e]r is for notes."))
  (ert-info ("End of line")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "l"))
      (should-error (execute-kbd-macro "10l"))))
  (ert-info ("Until end-of-line")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("100l")
      ";; This buffer is for notes[.]"))
  (ert-info ("On empty line")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      (should-error (execute-kbd-macro "l"))
      (should-error (execute-kbd-macro "42l")))))

(ert-deftest vimp-test-backward-char ()
  "Test `vimp-backward-char' motion"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; This[ ]buffer is for notes."
      ("h")
      ";; Thi[s] buffer is for notes."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; This[ ]buffer is for notes."
      ("3h")
      ";; T[h]is buffer is for notes."))
  (ert-info ("Beginning of line")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "h"))
      (should-error (execute-kbd-macro "10h"))))
  (ert-info ("Until beginning-of-line")
    (vimp-test-buffer
      ";; This[ ]buffer is for notes."
      ("100h")
      "[;]; This buffer is for notes."))
  (ert-info ("On empty line")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      (should-error (execute-kbd-macro "h"))
      (should-error (execute-kbd-macro "42h")))))

(ert-deftest vimp-test-previous-line ()
  "Test `vimp-previous-line' motion"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save,
;; [a]nd for Lisp evaluation."
      ("k")
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."
      ("2k")
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("Until beginning of buffer")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."
      ("100k")
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."))
  (ert-info ("At beginning of buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      (should-error (execute-kbd-macro "k"))
      (should-error (execute-kbd-macro "42k")))))

(ert-deftest vimp-test-next-line ()
  "Test `vimp-next-line' motion"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
      ("j")
      ";; This buffer is for notes you don't want to save,
;; [a]nd for Lisp evaluation."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("2j")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."))
  (ert-info ("Until end of buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
      ("100j")
      ";; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; [t]hen enter the text in that file's own buffer."))
  (ert-info ("At end of buffer")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to [s]ave."
      (should-error (execute-kbd-macro "j"))
      (should-error (execute-kbd-macro "42j")))))

(ert-deftest vimp-test-preserve-column ()
  "Test `vimp-previous-line' and `vimp-next-line' preserve the column."
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("j")
      "abc\nab[c]def\n\nabcd\n")
    (vimp-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jj")
      "abc\nabcdef\n[\n]abcd\n")
    (vimp-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jjj")
      "abc\nabcdef\n\nab[c]d\n")
    (vimp-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jjjk")
      "abc\nabcdef\n[\n]abcd\n")
    (vimp-test-buffer
      "ab[c]\nabcdef\n\nabcd\n"
      ("jjjkk")
      "abc\nab[c]def\n\nabcd\n")))

(ert-deftest vimp-test-beginning-of-line ()
  "Test `vimp-beginning-of-line' motion"
  :tags '(vimp motion)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save."
    ("0")
    "[;]; This buffer is for notes you don't want to save."
    ("0")
    "[;]; This buffer is for notes you don't want to save."))

(ert-deftest vimp-test-end-of-line ()
  "Test `vimp-end-of-line' motion"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; [T]his buffer is for notes you don't want to save."
      ("$")
      ";; This buffer is for notes you don't want to save[.]"
      ("$")
      ";; This buffer is for notes you don't want to save[.]"))
  (ert-info ("Don't delete blank lines")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      ("d$")
      "Above some line
\[]
Below some empty line")))

(ert-deftest vimp-test-first-non-blank ()
  "Test `vimp-first-non-blank' motion"
  :tags '(vimp motion)
  (vimp-test-buffer
    "\
  printf(\"Hello world\\n\")[;]
  return EXIT_SUCCESS;"
    ("^")
    "\
  [p]rintf(\"Hello world\\n\");
  return EXIT_SUCCESS;"
    ("j^")
    "\
  printf(\"Hello world\\n\");
  [r]eturn EXIT_SUCCESS;"))

(ert-deftest vimp-test-last-non-blank ()
  "Test `vimp-last-non-blank' motion"
  :tags '(vimp motion)
  (vimp-test-buffer
    "[i]nt main(int argc, char** argv)    \n\
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("g_")
    "int main(int argc, char** argv[)]    \n\
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("jjg_")
    "int main(int argc, char** argv)    \n\
{
  printf(\"Hello world\\n\")[;]
  return EXIT_SUCCESS;
}"))

(ert-deftest vimp-test-goto-first-line ()
  "Test `vimp-goto-first-line' motion"
  :tags '(vimp motion)
  (vimp-test-buffer
    "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("3gg")
    "int main(int argc, char** argv)
{
  [p]rintf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("gg")
    "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("100gg")
    "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"))

(ert-deftest vimp-test-goto-line ()
  "Test `vimp-goto-line' motion"
  :tags '(vimp motion)
  (vimp-test-buffer
    "[i]nt main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("G")
    "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"
    ("3G")
    "int main(int argc, char** argv)
{
  [p]rintf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
    ("100G")
    "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"))

(ert-deftest vimp-test-operator-0 ()
  "Test motion \"0\" with an operator."
  :tags '(vimp motion)
  (vimp-test-buffer
    ";; [T]his buffer is for notes."
    ("d0")
    "[T]his buffer is for notes."))

(ert-deftest vimp-test-forward-not-word ()
  "Test `vimp-forward-not-thing'"
  :tags '(vimp motion)
  (vimp-test-buffer
    "[ ]    aa,,"
    (vimp-forward-not-thing 'vimp-word)
    "     [a]a,,"))

;; TODO: test Visual motions and window motions
(ert-deftest vimp-test-forward-word-begin ()
  "Test `vimp-forward-word-begin'"
  :tags '(vimp motion)
  (ert-info ("Non-word")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("w")
      ";; [T]his buffer is for notes."))
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("w")
      ";; This [b]uffer is for notes."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("3w")
      ";; This buffer is [f]or notes."))
  (ert-info ("With count on whitespace")
    (vimp-test-buffer
      ";;[ ]This buffer is for notes."
      ("3w")
      ";; This buffer [i]s for notes."))
  (ert-info ("Empty line")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      ("w")
      "Above some line

\[B]elow some empty line")
    (vimp-test-buffer
      "[A]bove

Below some empty line"
      ("dw")
      "[]

Below some empty line"
      ("dw")
      "[]
Below some empty line"
      ("dw")
      "[B]elow some empty line")
    (vimp-test-buffer
      "[A]bove

    Below some empty line with leading whitespace"
      ("dw")
      "[]

    Below some empty line with leading whitespace"
      ("dw")
      "[]
    Below some empty line with leading whitespace"
      ("dw")
      "    [B]elow some empty line")
    (vimp-test-buffer
      "Some line with trailing whitespace  [ ]     \n    next line\n"
      ("dw")
      "Some line with trailing whitespace [ ]\n    next line\n")
    (vimp-test-buffer
      "[A]\n"
      ("dw")
      "[]\n"))
  (ert-info ("End of buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("100w")
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "w"))
      (should-error (execute-kbd-macro "10w"))))
  (ert-info ("Before last character in buffer")
    (vimp-test-buffer
      "fo[o]."
      ("w")
      "foo[.]")
    (vimp-test-buffer
      "fo[o] "
      ("w")
      "foo[ ]")
    (vimp-test-buffer
      "[ ]e"
      ("w")
      " [e]")))

(ert-deftest vimp-test-forward-word-end ()
  "Test `vimp-forward-word-end'"
  :tags '(vimp motion)
  (ert-info ("Non-word")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("e")
      ";[;] This buffer is for notes."))
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("e")
      ";; Thi[s] buffer is for notes."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("3e")
      ";; This buffer i[s] for notes."))
  (ert-info ("With count on whitespace")
    (vimp-test-buffer
      ";;[ ]This buffer is for notes."
      ("3e")
      ";; This buffer i[s] for notes."))
  (ert-info ("Delete")
    (vimp-test-buffer
      ";; This[-]buffer-is-for-notes."
      ("de")
      ";; This[-]is-for-notes."))
  (ert-info ("Empty line")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      ("e")
      "Above some line

Belo[w] some empty line"))
  (ert-info ("End of buffer")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("100e")
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "e"))
      (should-error (execute-kbd-macro "10e"))))
  ;; In Vim, "de" may delete two words rather than one
  ;; if the first word is only one letter. In Evil,
  ;; "de" always deletes one word.
  (ert-info ("Delete a single-letter word")
    (vimp-test-buffer
      "a [b] c"
      ("de")
      "a [ ]c")))

(ert-deftest vimp-test-backward-word-begin ()
  "Test `vimp-backward-word-begin'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("b")
      ";; This buffer is for [n]otes."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("2b")
      ";; This buffer is [f]or notes."))
  (ert-info ("Empty line")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      ("b")
      "Above some [l]ine

Below some empty line"))
  (ert-info ("With count on whitespace")
    (vimp-test-buffer
      ";; This buffer is for[ ]notes."
      ("2b")
      ";; This buffer [i]s for notes."))
  (ert-info ("Beginning of buffer")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("100b")
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "b"))
      (should-error (execute-kbd-macro "10b")))))

(ert-deftest vimp-test-backward-word-end ()
  "Test `vimp-backward-word-end'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("ge")
      ";; This buffer is for note[s]."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("2ge")
      ";; This buffer is fo[r] notes."))
  (ert-info ("Empty line")
    (vimp-test-buffer
      "Above some line
\[]
Below some empty line"
      ("ge")
      "Above some lin[e]

Below some empty line"))
  (ert-info ("With count on whitespace")
    (vimp-test-buffer
      ";; This buffer is for[ ]notes."
      ("2ge")
      ";; This buffer i[s] for notes."))
  (ert-info ("Beginning of buffer")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("100ge")
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "ge"))
      (should-error (execute-kbd-macro "10ge")))))

(ert-deftest vimp-test-forward-word-begin-cjk ()
  "Test `vimp-forward-word-begin' on CJK words"
  :tags '(vimp motion cjk)
  (ert-info ("Latin / numeric")
    (vimp-test-buffer
      "[a]bcd1234"
      ("w")
      "abcd123[4]"))
  (ert-info ("Latin / Kanji")
    (vimp-test-buffer
      "[a]bcd漢字"
      ("w")
      "abcd[漢]字"))
  (ert-info ("Latin / Hiragana")
    (vimp-test-buffer
      "[a]bcdひらがな"
      ("w")
      "abcd[ひ]らがな"))
  (ert-info ("Latin / Katakana")
    (vimp-test-buffer
      "[a]bcdカタカナ"
      ("w")
      "abcd[カ]タカナ"))
  (ert-info ("Latin / half-width Katakana")
    (vimp-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("w")
      "abcdｶﾀｶ[ﾅ]"))
  (ert-info ("Latin / full-width alphabet")
    (vimp-test-buffer
      "[a]bcdＡＢＣ"
      ("w")
      "abcdＡＢ[Ｃ]"))
  (ert-info ("Latin / full-width numeric")
    (vimp-test-buffer
      "[a]bcd１２３"
      ("w")
      "abcd１２[３]"))
  (ert-info ("Latin / Hangul")
    (vimp-test-buffer
      "[a]bcd한글"
      ("w")
      "abcd[한]글"))
  (ert-info ("numeric / Latin")
    (vimp-test-buffer
      "[1]234abcd"
      ("w")
      "1234abc[d]"))
  (ert-info ("numeric / Kanji")
    (vimp-test-buffer
      "[1]234漢字"
      ("w")
      "1234[漢]字"))
  (ert-info ("numeric / Hiragana")
    (vimp-test-buffer
      "[1]234ひらがな"
      ("w")
      "1234[ひ]らがな"))
  (ert-info ("numeric / Katakana")
    (vimp-test-buffer
      "[1]234カタカナ"
      ("w")
      "1234[カ]タカナ"))
  (ert-info ("numeric / half-width Katakana")
    (vimp-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("w")
      "1234ｶﾀｶ[ﾅ]"))
  (ert-info ("numeric / full-width alphabet")
    (vimp-test-buffer
      "[1]234ＡＢＣ"
      ("w")
      "1234ＡＢ[Ｃ]"))
  (ert-info ("numeric / full-width numeric")
    (vimp-test-buffer
      "[1]234１２３"
      ("w")
      "1234１２[３]"))
  (ert-info ("numeric / Hangul")
    (vimp-test-buffer
      "[1]234한글"
      ("w")
      "1234[한]글"))
  (ert-info ("Kanji / Latin")
    (vimp-test-buffer
      "[漢]字abcd"
      ("w")
      "漢字[a]bcd"))
  (ert-info ("Kanji / numeric")
    (vimp-test-buffer
      "[漢]字1234"
      ("w")
      "漢字[1]234"))
  (ert-info ("Kanji / Hiragana")
    (vimp-test-buffer
      "[漢]字ひらがな"
      ("w")
      "漢字[ひ]らがな"))
  (ert-info ("Kanji / Katakana")
    (vimp-test-buffer
      "[漢]字カタカナ"
      ("w")
      "漢字[カ]タカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (vimp-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("w")
      "漢字[ｶ]ﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (vimp-test-buffer
      "[漢]字ＡＢＣ"
      ("w")
      "漢字[Ａ]ＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (vimp-test-buffer
      "[漢]字１２３"
      ("w")
      "漢字[１]２３"))
  (ert-info ("Kanji / Hangul")
    (vimp-test-buffer
      "[漢]字한글"
      ("w")
      "漢字[한]글"))
  (ert-info ("Hiragana / Latin")
    (vimp-test-buffer
      "[ひ]らがなabcd"
      ("w")
      "ひらがな[a]bcd"))
  (ert-info ("Hiragana / numeric")
    (vimp-test-buffer
      "[ひ]らがな1234"
      ("w")
      "ひらがな[1]234"))
  (ert-info ("Hiragana / Kanji")
    (vimp-test-buffer
      "[ひ]らがな漢字"
      ("w")
      "ひらがな[漢]字"))
  (ert-info ("Hiragana / Katakana")
    (vimp-test-buffer
      "[ひ]らがなカタカナ"
      ("w")
      "ひらがな[カ]タカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (vimp-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("w")
      "ひらがな[ｶ]ﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (vimp-test-buffer
      "[ひ]らがなＡＢＣ"
      ("w")
      "ひらがな[Ａ]ＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (vimp-test-buffer
      "[ひ]らがな１２３"
      ("w")
      "ひらがな[１]２３"))
  (ert-info ("Hiragana / Hangul")
    (vimp-test-buffer
      "[ひ]らがな한글"
      ("w")
      "ひらがな[한]글"))
  (ert-info ("Katakana / Latin")
    (vimp-test-buffer
      "[カ]タカナabcd"
      ("w")
      "カタカナ[a]bcd"))
  (ert-info ("Katakana / numeric")
    (vimp-test-buffer
      "[カ]タカナ1234"
      ("w")
      "カタカナ[1]234"))
  (ert-info ("Katakana / Kanji")
    (vimp-test-buffer
      "[カ]タカナ漢字"
      ("w")
      "カタカナ[漢]字"))
  (ert-info ("Katakana / Hiragana")
    (vimp-test-buffer
      "[カ]タカナひらがな"
      ("w")
      "カタカナ[ひ]らがな"))
  (ert-info ("Katakana / half-width Katakana")
    (vimp-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("w")
      "カタカナ[ｶ]ﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (vimp-test-buffer
      "[カ]タカナＡＢＣ"
      ("w")
      "カタカナ[Ａ]ＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (vimp-test-buffer
      "[カ]タカナ１２３"
      ("w")
      "カタカナ[１]２３"))
  (ert-info ("Katakana / Hangul")
    (vimp-test-buffer
      "[カ]タカナ한글"
      ("w")
      "カタカナ[한]글"))
  (ert-info ("half-width Katakana / Latin")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("w")
      "ｶﾀｶﾅabc[d]"))
  (ert-info ("half-width Katakana / numeric")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("w")
      "ｶﾀｶﾅ123[4]"))
  (ert-info ("half-width Katakana / Kanji")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("w")
      "ｶﾀｶﾅ[漢]字"))
  (ert-info ("half-width Katakana / Hiragana")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("w")
      "ｶﾀｶﾅ[ひ]らがな"))
  (ert-info ("half-width Katakana / Katakana")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("w")
      "ｶﾀｶﾅ[カ]タカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("w")
      "ｶﾀｶﾅＡＢ[Ｃ]"))
  (ert-info ("half-width Katakana / full-width numeric")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("w")
      "ｶﾀｶﾅ１２[３]"))
  (ert-info ("half-width Katakana / Hangul")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("w")
      "ｶﾀｶﾅ[한]글"))
  (ert-info ("full-width alphabet / Latin")
    (vimp-test-buffer
      "[Ａ]ＢＣabcd"
      ("w")
      "ＡＢＣabc[d]"))
  (ert-info ("full-width alphabet / numeric")
    (vimp-test-buffer
      "[Ａ]ＢＣ1234"
      ("w")
      "ＡＢＣ123[4]"))
  (ert-info ("full-width alphabet / Kanji")
    (vimp-test-buffer
      "[Ａ]ＢＣ漢字"
      ("w")
      "ＡＢＣ[漢]字"))
  (ert-info ("full-width alphabet / Hiragana")
    (vimp-test-buffer
      "[Ａ]ＢＣひらがな"
      ("w")
      "ＡＢＣ[ひ]らがな"))
  (ert-info ("full-width alphabet / Katakana")
    (vimp-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("w")
      "ＡＢＣ[カ]タカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (vimp-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("w")
      "ＡＢＣｶﾀｶ[ﾅ]"))
  (ert-info ("full-width alphabet / full-width numeric")
    (vimp-test-buffer
      "[Ａ]ＢＣ１２３"
      ("w")
      "ＡＢＣ１２[３]"))
  (ert-info ("full-width alphabet / Hangul")
    (vimp-test-buffer
      "[Ａ]ＢＣ한글"
      ("w")
      "ＡＢＣ[한]글"))
  (ert-info ("full-width numeric / Latin")
    (vimp-test-buffer
      "[１]２３abcd"
      ("w")
      "１２３abc[d]"))
  (ert-info ("full-width numeric / numeric")
    (vimp-test-buffer
      "[１]２３1234"
      ("w")
      "１２３123[4]"))
  (ert-info ("full-width numeric / Kanji")
    (vimp-test-buffer
      "[１]２３漢字"
      ("w")
      "１２３[漢]字"))
  (ert-info ("full-width numeric / Hiragana")
    (vimp-test-buffer
      "[１]２３ひらがな"
      ("w")
      "１２３[ひ]らがな"))
  (ert-info ("full-width numeric / Katakana")
    (vimp-test-buffer
      "[１]２３カタカナ"
      ("w")
      "１２３[カ]タカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (vimp-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("w")
      "１２３ｶﾀｶ[ﾅ]"))
  (ert-info ("full-width numeric / full-width alphabet")
    (vimp-test-buffer
      "[１]２３ＡＢＣ"
      ("w")
      "１２３ＡＢ[Ｃ]"))
  (ert-info ("full-width numeric / Hangul")
    (vimp-test-buffer
      "[１]２３한글"
      ("w")
      "１２３[한]글"))
  (ert-info ("Hangul / Latin")
    (vimp-test-buffer
      "[한]글abcd"
      ("w")
      "한글[a]bcd"))
  (ert-info ("Hangul / numeric")
    (vimp-test-buffer
      "[한]글1234"
      ("w")
      "한글[1]234"))
  (ert-info ("Hangul / Kanji")
    (vimp-test-buffer
      "[한]글漢字"
      ("w")
      "한글[漢]字"))
  (ert-info ("Hangul / Hiragana")
    (vimp-test-buffer
      "[한]글ひらがな"
      ("w")
      "한글[ひ]らがな"))
  (ert-info ("Hangul / Katakana")
    (vimp-test-buffer
      "[한]글カタカナ"
      ("w")
      "한글[カ]タカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (vimp-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("w")
      "한글[ｶ]ﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (vimp-test-buffer
      "[한]글ＡＢＣ"
      ("w")
      "한글[Ａ]ＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (vimp-test-buffer
      "[한]글１２３"
      ("w")
      "한글[１]２３")))

(ert-deftest vimp-test-forward-word-end-cjk ()
  "Test `vimp-forward-word-end' on CJK words"
  :tags '(vimp motion cjk)
  (ert-info ("Latin / numeric")
    (vimp-test-buffer
      "[a]bcd1234"
      ("e")
      "abcd123[4]"))
  (ert-info ("Latin / Kanji")
    (vimp-test-buffer
      "[a]bcd漢字"
      ("e")
      "abc[d]漢字"))
  (ert-info ("Latin / Hiragana")
    (vimp-test-buffer
      "[a]bcdひらがな"
      ("e")
      "abc[d]ひらがな"))
  (ert-info ("Latin / Katakana")
    (vimp-test-buffer
      "[a]bcdカタカナ"
      ("e")
      "abc[d]カタカナ"))
  (ert-info ("Latin / half-width Katakana")
    (vimp-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("e")
      "abcdｶﾀｶ[ﾅ]"))
  (ert-info ("Latin / full-width alphabet")
    (vimp-test-buffer
      "[a]bcdＡＢＣ"
      ("e")
      "abcdＡＢ[Ｃ]"))
  (ert-info ("Latin / full-width numeric")
    (vimp-test-buffer
      "[a]bcd１２３"
      ("e")
      "abcd１２[３]"))
  (ert-info ("Latin / Hangul")
    (vimp-test-buffer
      "[a]bcd한글"
      ("e")
      "abc[d]한글"))
  (ert-info ("numeric / Latin")
    (vimp-test-buffer
      "[1]234abcd"
      ("e")
      "1234abc[d]"))
  (ert-info ("numeric / Kanji")
    (vimp-test-buffer
      "[1]234漢字"
      ("e")
      "123[4]漢字"))
  (ert-info ("numeric / Hiragana")
    (vimp-test-buffer
      "[1]234ひらがな"
      ("e")
      "123[4]ひらがな"))
  (ert-info ("numeric / Katakana")
    (vimp-test-buffer
      "[1]234カタカナ"
      ("e")
      "123[4]カタカナ"))
  (ert-info ("numeric / half-width Katakana")
    (vimp-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("e")
      "1234ｶﾀｶ[ﾅ]"))
  (ert-info ("numeric / full-width alphabet")
    (vimp-test-buffer
      "[1]234ＡＢＣ"
      ("e")
      "1234ＡＢ[Ｃ]"))
  (ert-info ("numeric / full-width numeric")
    (vimp-test-buffer
      "[1]234１２３"
      ("e")
      "1234１２[３]"))
  (ert-info ("numeric / Hangul")
    (vimp-test-buffer
      "[1]234한글"
      ("e")
      "123[4]한글"))
  (ert-info ("Kanji / Latin")
    (vimp-test-buffer
      "[漢]字abcd"
      ("e")
      "漢[字]abcd"))
  (ert-info ("Kanji / numeric")
    (vimp-test-buffer
      "[漢]字1234"
      ("e")
      "漢[字]1234"))
  (ert-info ("Kanji / Hiragana")
    (vimp-test-buffer
      "[漢]字ひらがな"
      ("e")
      "漢[字]ひらがな"))
  (ert-info ("Kanji / Katakana")
    (vimp-test-buffer
      "[漢]字カタカナ"
      ("e")
      "漢[字]カタカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (vimp-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("e")
      "漢[字]ｶﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (vimp-test-buffer
      "[漢]字ＡＢＣ"
      ("e")
      "漢[字]ＡＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (vimp-test-buffer
      "[漢]字１２３"
      ("e")
      "漢[字]１２３"))
  (ert-info ("Kanji / Hangul")
    (vimp-test-buffer
      "[漢]字한글"
      ("e")
      "漢[字]한글"))
  (ert-info ("Hiragana / Latin")
    (vimp-test-buffer
      "[ひ]らがなabcd"
      ("e")
      "ひらが[な]abcd"))
  (ert-info ("Hiragana / numeric")
    (vimp-test-buffer
      "[ひ]らがな1234"
      ("e")
      "ひらが[な]1234"))
  (ert-info ("Hiragana / Kanji")
    (vimp-test-buffer
      "[ひ]らがな漢字"
      ("e")
      "ひらが[な]漢字"))
  (ert-info ("Hiragana / Katakana")
    (vimp-test-buffer
      "[ひ]らがなカタカナ"
      ("e")
      "ひらが[な]カタカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (vimp-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("e")
      "ひらが[な]ｶﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (vimp-test-buffer
      "[ひ]らがなＡＢＣ"
      ("e")
      "ひらが[な]ＡＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (vimp-test-buffer
      "[ひ]らがな１２３"
      ("e")
      "ひらが[な]１２３"))
  (ert-info ("Hiragana / Hangul")
    (vimp-test-buffer
      "[ひ]らがな한글"
      ("e")
      "ひらが[な]한글"))
  (ert-info ("Katakana / Latin")
    (vimp-test-buffer
      "[カ]タカナabcd"
      ("e")
      "カタカ[ナ]abcd"))
  (ert-info ("Katakana / numeric")
    (vimp-test-buffer
      "[カ]タカナ1234"
      ("e")
      "カタカ[ナ]1234"))
  (ert-info ("Katakana / Kanji")
    (vimp-test-buffer
      "[カ]タカナ漢字"
      ("e")
      "カタカ[ナ]漢字"))
  (ert-info ("Katakana / Hiragana")
    (vimp-test-buffer
      "[カ]タカナひらがな"
      ("e")
      "カタカ[ナ]ひらがな"))
  (ert-info ("Katakana / half-width Katakana")
    (vimp-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("e")
      "カタカ[ナ]ｶﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (vimp-test-buffer
      "[カ]タカナＡＢＣ"
      ("e")
      "カタカ[ナ]ＡＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (vimp-test-buffer
      "[カ]タカナ１２３"
      ("e")
      "カタカ[ナ]１２３"))
  (ert-info ("Katakana / Hangul")
    (vimp-test-buffer
      "[カ]タカナ한글"
      ("e")
      "カタカ[ナ]한글"))
  (ert-info ("half-width Katakana / Latin")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("e")
      "ｶﾀｶﾅabc[d]"))
  (ert-info ("half-width Katakana / numeric")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("e")
      "ｶﾀｶﾅ123[4]"))
  (ert-info ("half-width Katakana / Kanji")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("e")
      "ｶﾀｶ[ﾅ]漢字"))
  (ert-info ("half-width Katakana / Hiragana")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("e")
      "ｶﾀｶ[ﾅ]ひらがな"))
  (ert-info ("half-width Katakana / Katakana")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("e")
      "ｶﾀｶ[ﾅ]カタカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("e")
      "ｶﾀｶﾅＡＢ[Ｃ]"))
  (ert-info ("half-width Katakana / full-width numeric")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("e")
      "ｶﾀｶﾅ１２[３]"))
  (ert-info ("half-width Katakana / Hangul")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("e")
      "ｶﾀｶ[ﾅ]한글"))
  (ert-info ("full-width alphabet / Latin")
    (vimp-test-buffer
      "[Ａ]ＢＣabcd"
      ("e")
      "ＡＢＣabc[d]"))
  (ert-info ("full-width alphabet / numeric")
    (vimp-test-buffer
      "[Ａ]ＢＣ1234"
      ("e")
      "ＡＢＣ123[4]"))
  (ert-info ("full-width alphabet / Kanji")
    (vimp-test-buffer
      "[Ａ]ＢＣ漢字"
      ("e")
      "ＡＢ[Ｃ]漢字"))
  (ert-info ("full-width alphabet / Hiragana")
    (vimp-test-buffer
      "[Ａ]ＢＣひらがな"
      ("e")
      "ＡＢ[Ｃ]ひらがな"))
  (ert-info ("full-width alphabet / Katakana")
    (vimp-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("e")
      "ＡＢ[Ｃ]カタカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (vimp-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("e")
      "ＡＢＣｶﾀｶ[ﾅ]"))
  (ert-info ("full-width alphabet / full-width numeric")
    (vimp-test-buffer
      "[Ａ]ＢＣ１２３"
      ("e")
      "ＡＢＣ１２[３]"))
  (ert-info ("full-width alphabet / Hangul")
    (vimp-test-buffer
      "[Ａ]ＢＣ한글"
      ("e")
      "ＡＢ[Ｃ]한글"))
  (ert-info ("full-width numeric / Latin")
    (vimp-test-buffer
      "[１]２３abcd"
      ("e")
      "１２３abc[d]"))
  (ert-info ("full-width numeric / numeric")
    (vimp-test-buffer
      "[１]２３1234"
      ("e")
      "１２３123[4]"))
  (ert-info ("full-width numeric / Kanji")
    (vimp-test-buffer
      "[１]２３漢字"
      ("e")
      "１２[３]漢字"))
  (ert-info ("full-width numeric / Hiragana")
    (vimp-test-buffer
      "[１]２３ひらがな"
      ("e")
      "１２[３]ひらがな"))
  (ert-info ("full-width numeric / Katakana")
    (vimp-test-buffer
      "[１]２３カタカナ"
      ("e")
      "１２[３]カタカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (vimp-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("e")
      "１２３ｶﾀｶ[ﾅ]"))
  (ert-info ("full-width numeric / full-width alphabet")
    (vimp-test-buffer
      "[１]２３ＡＢＣ"
      ("e")
      "１２３ＡＢ[Ｃ]"))
  (ert-info ("full-width numeric / Hangul")
    (vimp-test-buffer
      "[１]２３한글"
      ("e")
      "１２[３]한글"))
  (ert-info ("Hangul / Latin")
    (vimp-test-buffer
      "[한]글abcd"
      ("e")
      "한[글]abcd"))
  (ert-info ("Hangul / numeric")
    (vimp-test-buffer
      "[한]글1234"
      ("e")
      "한[글]1234"))
  (ert-info ("Hangul / Kanji")
    (vimp-test-buffer
      "[한]글漢字"
      ("e")
      "한[글]漢字"))
  (ert-info ("Hangul / Hiragana")
    (vimp-test-buffer
      "[한]글ひらがな"
      ("e")
      "한[글]ひらがな"))
  (ert-info ("Hangul / Katakana")
    (vimp-test-buffer
      "[한]글カタカナ"
      ("e")
      "한[글]カタカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (vimp-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("e")
      "한[글]ｶﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (vimp-test-buffer
      "[한]글ＡＢＣ"
      ("e")
      "한[글]ＡＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (vimp-test-buffer
      "[한]글１２３"
      ("e")
      "한[글]１２３")))

(ert-deftest vimp-test-backword-word-begin-cjk ()
  "Test `vimp-backward-word-begin' on CJK words"
  :tags '(vimp motion cjk)
  (ert-info ("Latin / numeric")
    (vimp-test-buffer
      "abcd123[4]"
      ("b")
      "[a]bcd1234"))
  (ert-info ("Latin / Kanji")
    (vimp-test-buffer
      "abcd漢[字]"
      ("b")
      "abcd[漢]字"))
  (ert-info ("Latin / Hiragana")
    (vimp-test-buffer
      "abcdひらが[な]"
      ("b")
      "abcd[ひ]らがな"))
  (ert-info ("Latin / Katakana")
    (vimp-test-buffer
      "abcdカタカ[ナ]"
      ("b")
      "abcd[カ]タカナ"))
  (ert-info ("Latin / half-width Katakana")
    (vimp-test-buffer
      "abcdｶﾀｶ[ﾅ]"
      ("b")
      "[a]bcdｶﾀｶﾅ"))
  (ert-info ("Latin / full-width alphabet")
    (vimp-test-buffer
      "abcdＡＢ[Ｃ]"
      ("b")
      "[a]bcdＡＢＣ"))
  (ert-info ("Latin / full-width numeric")
    (vimp-test-buffer
      "abcd１２[３]"
      ("b")
      "[a]bcd１２３"))
  (ert-info ("Latin / Hangul")
    (vimp-test-buffer
      "abcd한[글]"
      ("b")
      "abcd[한]글"))
  (ert-info ("numeric / Latin")
    (vimp-test-buffer
      "1234abc[d]"
      ("b")
      "[1]234abcd"))
  (ert-info ("numeric / Kanji")
    (vimp-test-buffer
      "1234漢[字]"
      ("b")
      "1234[漢]字"))
  (ert-info ("numeric / Hiragana")
    (vimp-test-buffer
      "1234ひらが[な]"
      ("b")
      "1234[ひ]らがな"))
  (ert-info ("numeric / Katakana")
    (vimp-test-buffer
      "1234カタカ[ナ]"
      ("b")
      "1234[カ]タカナ"))
  (ert-info ("numeric / half-width Katakana")
    (vimp-test-buffer
      "1234ｶﾀｶ[ﾅ]"
      ("b")
      "[1]234ｶﾀｶﾅ"))
  (ert-info ("numeric / full-width alphabet")
    (vimp-test-buffer
      "1234ＡＢ[Ｃ]"
      ("b")
      "[1]234ＡＢＣ"))
  (ert-info ("numeric / full-width numeric")
    (vimp-test-buffer
      "1234１２[３]"
      ("b")
      "[1]234１２３"))
  (ert-info ("numeric / Hangul")
    (vimp-test-buffer
      "1234한[글]"
      ("b")
      "1234[한]글"))
  (ert-info ("Kanji / Latin")
    (vimp-test-buffer
      "漢字abc[d]"
      ("b")
      "漢字[a]bcd"))
  (ert-info ("Kanji / numeric")
    (vimp-test-buffer
      "漢字123[4]"
      ("b")
      "漢字[1]234"))
  (ert-info ("Kanji / Hiragana")
    (vimp-test-buffer
      "漢字ひらが[な]"
      ("b")
      "漢字[ひ]らがな"))
  (ert-info ("Kanji / Katakana")
    (vimp-test-buffer
      "漢字カタカ[ナ]"
      ("b")
      "漢字[カ]タカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (vimp-test-buffer
      "漢字ｶﾀｶ[ﾅ]"
      ("b")
      "漢字[ｶ]ﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (vimp-test-buffer
      "漢字ＡＢ[Ｃ]"
      ("b")
      "漢字[Ａ]ＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (vimp-test-buffer
      "漢字１２[３]"
      ("b")
      "漢字[１]２３"))
  (ert-info ("Kanji / Hangul")
    (vimp-test-buffer
      "漢字한[글]"
      ("b")
      "漢字[한]글"))
  (ert-info ("Hiragana / Latin")
    (vimp-test-buffer
      "ひらがなabc[d]"
      ("b")
      "ひらがな[a]bcd"))
  (ert-info ("Hiragana / numeric")
    (vimp-test-buffer
      "ひらがな123[4]"
      ("b")
      "ひらがな[1]234"))
  (ert-info ("Hiragana / Kanji")
    (vimp-test-buffer
      "ひらがな漢[字]"
      ("b")
      "ひらがな[漢]字"))
  (ert-info ("Hiragana / Katakana")
    (vimp-test-buffer
      "ひらがなカタカ[ナ]"
      ("b")
      "ひらがな[カ]タカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (vimp-test-buffer
      "ひらがなｶﾀｶ[ﾅ]"
      ("b")
      "ひらがな[ｶ]ﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (vimp-test-buffer
      "ひらがなＡＢ[Ｃ]"
      ("b")
      "ひらがな[Ａ]ＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (vimp-test-buffer
      "ひらがな１２[３]"
      ("b")
      "ひらがな[１]２３"))
  (ert-info ("Hiragana / Hangul")
    (vimp-test-buffer
      "ひらがな한[글]"
      ("b")
      "ひらがな[한]글"))
  (ert-info ("Katakana / Latin")
    (vimp-test-buffer
      "カタカナabc[d]"
      ("b")
      "カタカナ[a]bcd"))
  (ert-info ("Katakana / numeric")
    (vimp-test-buffer
      "カタカナ123[4]"
      ("b")
      "カタカナ[1]234"))
  (ert-info ("Katakana / Kanji")
    (vimp-test-buffer
      "カタカナ漢[字]"
      ("b")
      "カタカナ[漢]字"))
  (ert-info ("Katakana / Hiragana")
    (vimp-test-buffer
      "カタカナひらが[な]"
      ("b")
      "カタカナ[ひ]らがな"))
  (ert-info ("Katakana / half-width Katakana")
    (vimp-test-buffer
      "カタカナｶﾀｶ[ﾅ]"
      ("b")
      "カタカナ[ｶ]ﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (vimp-test-buffer
      "カタカナＡＢ[Ｃ]"
      ("b")
      "カタカナ[Ａ]ＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (vimp-test-buffer
      "カタカナ１２[３]"
      ("b")
      "カタカナ[１]２３"))
  (ert-info ("Katakana / Hangul")
    (vimp-test-buffer
      "カタカナ한[글]"
      ("b")
      "カタカナ[한]글"))
  (ert-info ("half-width Katakana / Latin")
    (vimp-test-buffer
      "ｶﾀｶﾅabc[d]"
      ("b")
      "[ｶ]ﾀｶﾅabcd"))
  (ert-info ("half-width Katakana / numeric")
    (vimp-test-buffer
      "ｶﾀｶﾅ123[4]"
      ("b")
      "[ｶ]ﾀｶﾅ1234"))
  (ert-info ("half-width Katakana / Kanji")
    (vimp-test-buffer
      "ｶﾀｶﾅ漢[字]"
      ("b")
      "ｶﾀｶﾅ[漢]字"))
  (ert-info ("half-width Katakana / Hiragana")
    (vimp-test-buffer
      "ｶﾀｶﾅひらが[な]"
      ("b")
      "ｶﾀｶﾅ[ひ]らがな"))
  (ert-info ("half-width Katakana / Katakana")
    (vimp-test-buffer
      "ｶﾀｶﾅカタカ[ナ]"
      ("b")
      "ｶﾀｶﾅ[カ]タカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (vimp-test-buffer
      "ｶﾀｶﾅＡＢ[Ｃ]"
      ("b")
      "[ｶ]ﾀｶﾅＡＢＣ"))
  (ert-info ("half-width Katakana / full-width numeric")
    (vimp-test-buffer
      "ｶﾀｶﾅ１２[３]"
      ("b")
      "[ｶ]ﾀｶﾅ１２３"))
  (ert-info ("half-width Katakana / Hangul")
    (vimp-test-buffer
      "ｶﾀｶﾅ한[글]"
      ("b")
      "ｶﾀｶﾅ[한]글"))
  (ert-info ("full-width alphabet / Latin")
    (vimp-test-buffer
      "ＡＢＣabc[d]"
      ("b")
      "[Ａ]ＢＣabcd"))
  (ert-info ("full-width alphabet / numeric")
    (vimp-test-buffer
      "ＡＢＣ123[4]"
      ("b")
      "[Ａ]ＢＣ1234"))
  (ert-info ("full-width alphabet / Kanji")
    (vimp-test-buffer
      "ＡＢＣ漢[字]"
      ("b")
      "ＡＢＣ[漢]字"))
  (ert-info ("full-width alphabet / Hiragana")
    (vimp-test-buffer
      "ＡＢＣひらが[な]"
      ("b")
      "ＡＢＣ[ひ]らがな"))
  (ert-info ("full-width alphabet / Katakana")
    (vimp-test-buffer
      "ＡＢＣカタカ[ナ]"
      ("b")
      "ＡＢＣ[カ]タカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (vimp-test-buffer
      "ＡＢＣｶﾀｶ[ﾅ]"
      ("b")
      "[Ａ]ＢＣｶﾀｶﾅ"))
  (ert-info ("full-width alphabet / full-width numeric")
    (vimp-test-buffer
      "ＡＢＣ１２[３]"
      ("b")
      "[Ａ]ＢＣ１２３"))
  (ert-info ("full-width alphabet / Hangul")
    (vimp-test-buffer
      "ＡＢＣ한[글]"
      ("b")
      "ＡＢＣ[한]글"))
  (ert-info ("full-width numeric / Latin")
    (vimp-test-buffer
      "１２３abc[d]"
      ("b")
      "[１]２３abcd"))
  (ert-info ("full-width numeric / numeric")
    (vimp-test-buffer
      "１２３123[4]"
      ("b")
      "[１]２３1234"))
  (ert-info ("full-width numeric / Kanji")
    (vimp-test-buffer
      "１２３漢[字]"
      ("b")
      "１２３[漢]字"))
  (ert-info ("full-width numeric / Hiragana")
    (vimp-test-buffer
      "１２３ひらが[な]"
      ("b")
      "１２３[ひ]らがな"))
  (ert-info ("full-width numeric / Katakana")
    (vimp-test-buffer
      "１２３カタカ[ナ]"
      ("b")
      "１２３[カ]タカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (vimp-test-buffer
      "１２３ｶﾀｶ[ﾅ]"
      ("b")
      "[１]２３ｶﾀｶﾅ"))
  (ert-info ("full-width numeric / full-width alphabet")
    (vimp-test-buffer
      "１２３ＡＢ[Ｃ]"
      ("b")
      "[１]２３ＡＢＣ"))
  (ert-info ("full-width numeric / Hangul")
    (vimp-test-buffer
      "１２３한[글]"
      ("b")
      "１２３[한]글"))
  (ert-info ("Hangul / Latin")
    (vimp-test-buffer
      "한글abc[d]"
      ("b")
      "한글[a]bcd"))
  (ert-info ("Hangul / numeric")
    (vimp-test-buffer
      "한글123[4]"
      ("b")
      "한글[1]234"))
  (ert-info ("Hangul / Kanji")
    (vimp-test-buffer
      "한글漢[字]"
      ("b")
      "한글[漢]字"))
  (ert-info ("Hangul / Hiragana")
    (vimp-test-buffer
      "한글ひらが[な]"
      ("b")
      "한글[ひ]らがな"))
  (ert-info ("Hangul / Katakana")
    (vimp-test-buffer
      "한글カタカ[ナ]"
      ("b")
      "한글[カ]タカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (vimp-test-buffer
      "한글ｶﾀｶ[ﾅ]"
      ("b")
      "한글[ｶ]ﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (vimp-test-buffer
      "한글ＡＢ[Ｃ]"
      ("b")
      "한글[Ａ]ＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (vimp-test-buffer
      "한글１２[３]"
      ("b")
      "한글[１]２３")))

(ert-deftest vimp-test-backword-word-end-cjk ()
  "Test `vimp-backward-word-end' on CJK words"
  :tags '(vimp motion cjk)
  (ert-info ("Latin / numeric")
    (vimp-test-buffer
      "abcd123[4]"
      ("ge")
      "[a]bcd1234"))
  (ert-info ("Latin / Kanji")
    (vimp-test-buffer
      "abcd漢[字]"
      ("ge")
      "abc[d]漢字"))
  (ert-info ("Latin / Hiragana")
    (vimp-test-buffer
      "abcdひらが[な]"
      ("ge")
      "abc[d]ひらがな"))
  (ert-info ("Latin / Katakana")
    (vimp-test-buffer
      "abcdカタカ[ナ]"
      ("ge")
      "abc[d]カタカナ"))
  (ert-info ("Latin / half-width Katakana")
    (vimp-test-buffer
      "abcdｶﾀｶ[ﾅ]"
      ("ge")
      "[a]bcdｶﾀｶﾅ"))
  (ert-info ("Latin / full-width alphabet")
    (vimp-test-buffer
      "abcdＡＢ[Ｃ]"
      ("ge")
      "[a]bcdＡＢＣ"))
  (ert-info ("Latin / full-width numeric")
    (vimp-test-buffer
      "abcd１２[３]"
      ("ge")
      "[a]bcd１２３"))
  (ert-info ("Latin / Hangul")
    (vimp-test-buffer
      "abcd한[글]"
      ("ge")
      "abc[d]한글"))
  (ert-info ("numeric / Latin")
    (vimp-test-buffer
      "1234abc[d]"
      ("ge")
      "[1]234abcd"))
  (ert-info ("numeric / Kanji")
    (vimp-test-buffer
      "1234漢[字]"
      ("ge")
      "123[4]漢字"))
  (ert-info ("numeric / Hiragana")
    (vimp-test-buffer
      "1234ひらが[な]"
      ("ge")
      "123[4]ひらがな"))
  (ert-info ("numeric / Katakana")
    (vimp-test-buffer
      "1234カタカ[ナ]"
      ("ge")
      "123[4]カタカナ"))
  (ert-info ("numeric / half-width Katakana")
    (vimp-test-buffer
      "1234ｶﾀｶ[ﾅ]"
      ("ge")
      "[1]234ｶﾀｶﾅ"))
  (ert-info ("numeric / full-width alphabet")
    (vimp-test-buffer
      "1234ＡＢ[Ｃ]"
      ("ge")
      "[1]234ＡＢＣ"))
  (ert-info ("numeric / full-width numeric")
    (vimp-test-buffer
      "1234１２[３]"
      ("ge")
      "[1]234１２３"))
  (ert-info ("numeric / Hangul")
    (vimp-test-buffer
      "1234한[글]"
      ("ge")
      "123[4]한글"))
  (ert-info ("Kanji / Latin")
    (vimp-test-buffer
      "漢字abc[d]"
      ("ge")
      "漢[字]abcd"))
  (ert-info ("Kanji / numeric")
    (vimp-test-buffer
      "漢字123[4]"
      ("ge")
      "漢[字]1234"))
  (ert-info ("Kanji / Hiragana")
    (vimp-test-buffer
      "漢字ひらが[な]"
      ("ge")
      "漢[字]ひらがな"))
  (ert-info ("Kanji / Katakana")
    (vimp-test-buffer
      "漢字カタカ[ナ]"
      ("ge")
      "漢[字]カタカナ"))
  (ert-info ("Kanji / half-width Katakana")
    (vimp-test-buffer
      "漢字ｶﾀｶ[ﾅ]"
      ("ge")
      "漢[字]ｶﾀｶﾅ"))
  (ert-info ("Kanji / full-width alphabet")
    (vimp-test-buffer
      "漢字ＡＢ[Ｃ]"
      ("ge")
      "漢[字]ＡＢＣ"))
  (ert-info ("Kanji / full-width numeric")
    (vimp-test-buffer
      "漢字１２[３]"
      ("ge")
      "漢[字]１２３"))
  (ert-info ("Kanji / Hangul")
    (vimp-test-buffer
      "漢字한[글]"
      ("ge")
      "漢[字]한글"))
  (ert-info ("Hiragana / Latin")
    (vimp-test-buffer
      "ひらがなabc[d]"
      ("ge")
      "ひらが[な]abcd"))
  (ert-info ("Hiragana / numeric")
    (vimp-test-buffer
      "ひらがな123[4]"
      ("ge")
      "ひらが[な]1234"))
  (ert-info ("Hiragana / Kanji")
    (vimp-test-buffer
      "ひらがな漢[字]"
      ("ge")
      "ひらが[な]漢字"))
  (ert-info ("Hiragana / Katakana")
    (vimp-test-buffer
      "ひらがなカタカ[ナ]"
      ("ge")
      "ひらが[な]カタカナ"))
  (ert-info ("Hiragana / half-width Katakana")
    (vimp-test-buffer
      "ひらがなｶﾀｶ[ﾅ]"
      ("ge")
      "ひらが[な]ｶﾀｶﾅ"))
  (ert-info ("Hiragana / full-width alphabet")
    (vimp-test-buffer
      "ひらがなＡＢ[Ｃ]"
      ("ge")
      "ひらが[な]ＡＢＣ"))
  (ert-info ("Hiragana / full-width numeric")
    (vimp-test-buffer
      "ひらがな１２[３]"
      ("ge")
      "ひらが[な]１２３"))
  (ert-info ("Hiragana / Hangul")
    (vimp-test-buffer
      "ひらがな한[글]"
      ("ge")
      "ひらが[な]한글"))
  (ert-info ("Katakana / Latin")
    (vimp-test-buffer
      "カタカナabc[d]"
      ("ge")
      "カタカ[ナ]abcd"))
  (ert-info ("Katakana / numeric")
    (vimp-test-buffer
      "カタカナ123[4]"
      ("ge")
      "カタカ[ナ]1234"))
  (ert-info ("Katakana / Kanji")
    (vimp-test-buffer
      "カタカナ漢[字]"
      ("ge")
      "カタカ[ナ]漢字"))
  (ert-info ("Katakana / Hiragana")
    (vimp-test-buffer
      "カタカナひらが[な]"
      ("ge")
      "カタカ[ナ]ひらがな"))
  (ert-info ("Katakana / half-width Katakana")
    (vimp-test-buffer
      "カタカナｶﾀｶ[ﾅ]"
      ("ge")
      "カタカ[ナ]ｶﾀｶﾅ"))
  (ert-info ("Katakana / full-width alphabet")
    (vimp-test-buffer
      "カタカナＡＢ[Ｃ]"
      ("ge")
      "カタカ[ナ]ＡＢＣ"))
  (ert-info ("Katakana / full-width numeric")
    (vimp-test-buffer
      "カタカナ１２[３]"
      ("ge")
      "カタカ[ナ]１２３"))
  (ert-info ("Katakana / Hangul")
    (vimp-test-buffer
      "カタカナ한[글]"
      ("ge")
      "カタカ[ナ]한글"))
  (ert-info ("half-width Katakana / Latin")
    (vimp-test-buffer
      "ｶﾀｶﾅabc[d]"
      ("ge")
      "[ｶ]ﾀｶﾅabcd"))
  (ert-info ("half-width Katakana / numeric")
    (vimp-test-buffer
      "ｶﾀｶﾅ123[4]"
      ("ge")
      "[ｶ]ﾀｶﾅ1234"))
  (ert-info ("half-width Katakana / Kanji")
    (vimp-test-buffer
      "ｶﾀｶﾅ漢[字]"
      ("ge")
      "ｶﾀｶ[ﾅ]漢字"))
  (ert-info ("half-width Katakana / Hiragana")
    (vimp-test-buffer
      "ｶﾀｶﾅひらが[な]"
      ("ge")
      "ｶﾀｶ[ﾅ]ひらがな"))
  (ert-info ("half-width Katakana / Katakana")
    (vimp-test-buffer
      "ｶﾀｶﾅカタカ[ナ]"
      ("ge")
      "ｶﾀｶ[ﾅ]カタカナ"))
  (ert-info ("half-width Katakana / full-width alphabet")
    (vimp-test-buffer
      "ｶﾀｶﾅＡＢ[Ｃ]"
      ("ge")
      "[ｶ]ﾀｶﾅＡＢＣ"))
  (ert-info ("half-width Katakana / full-width numeric")
    (vimp-test-buffer
      "ｶﾀｶﾅ１２[３]"
      ("ge")
      "[ｶ]ﾀｶﾅ１２３"))
  (ert-info ("half-width Katakana / Hangul")
    (vimp-test-buffer
      "ｶﾀｶﾅ한[글]"
      ("ge")
      "ｶﾀｶ[ﾅ]한글"))
  (ert-info ("full-width alphabet / Latin")
    (vimp-test-buffer
      "ＡＢＣabc[d]"
      ("ge")
      "[Ａ]ＢＣabcd"))
  (ert-info ("full-width alphabet / numeric")
    (vimp-test-buffer
      "ＡＢＣ123[4]"
      ("ge")
      "[Ａ]ＢＣ1234"))
  (ert-info ("full-width alphabet / Kanji")
    (vimp-test-buffer
      "ＡＢＣ漢[字]"
      ("ge")
      "ＡＢ[Ｃ]漢字"))
  (ert-info ("full-width alphabet / Hiragana")
    (vimp-test-buffer
      "ＡＢＣひらが[な]"
      ("ge")
      "ＡＢ[Ｃ]ひらがな"))
  (ert-info ("full-width alphabet / Katakana")
    (vimp-test-buffer
      "ＡＢＣカタカ[ナ]"
      ("ge")
      "ＡＢ[Ｃ]カタカナ"))
  (ert-info ("full-width alphabet / half-width Katakana")
    (vimp-test-buffer
      "ＡＢＣｶﾀｶ[ﾅ]"
      ("ge")
      "[Ａ]ＢＣｶﾀｶﾅ"))
  (ert-info ("full-width alphabet / full-width numeric")
    (vimp-test-buffer
      "ＡＢＣ１２[３]"
      ("ge")
      "[Ａ]ＢＣ１２３"))
  (ert-info ("full-width alphabet / Hangul")
    (vimp-test-buffer
      "ＡＢＣ한[글]"
      ("ge")
      "ＡＢ[Ｃ]한글"))
  (ert-info ("full-width numeric / Latin")
    (vimp-test-buffer
      "１２３abc[d]"
      ("ge")
      "[１]２３abcd"))
  (ert-info ("full-width numeric / numeric")
    (vimp-test-buffer
      "１２３123[4]"
      ("ge")
      "[１]２３1234"))
  (ert-info ("full-width numeric / Kanji")
    (vimp-test-buffer
      "１２３漢[字]"
      ("ge")
      "１２[３]漢字"))
  (ert-info ("full-width numeric / Hiragana")
    (vimp-test-buffer
      "１２３ひらが[な]"
      ("ge")
      "１２[３]ひらがな"))
  (ert-info ("full-width numeric / Katakana")
    (vimp-test-buffer
      "１２３カタカ[ナ]"
      ("ge")
      "１２[３]カタカナ"))
  (ert-info ("full-width numeric / half-width Katakana")
    (vimp-test-buffer
      "１２３ｶﾀｶ[ﾅ]"
      ("ge")
      "[１]２３ｶﾀｶﾅ"))
  (ert-info ("full-width numeric / full-width alphabet")
    (vimp-test-buffer
      "１２３ＡＢ[Ｃ]"
      ("ge")
      "[１]２３ＡＢＣ"))
  (ert-info ("full-width numeric / Hangul")
    (vimp-test-buffer
      "１２３한[글]"
      ("ge")
      "１２[３]한글"))
  (ert-info ("Hangul / Latin")
    (vimp-test-buffer
      "한글abc[d]"
      ("ge")
      "한[글]abcd"))
  (ert-info ("Hangul / numeric")
    (vimp-test-buffer
      "한글123[4]"
      ("ge")
      "한[글]1234"))
  (ert-info ("Hangul / Kanji")
    (vimp-test-buffer
      "한글漢[字]"
      ("ge")
      "한[글]漢字"))
  (ert-info ("Hangul / Hiragana")
    (vimp-test-buffer
      "한글ひらが[な]"
      ("ge")
      "한[글]ひらがな"))
  (ert-info ("Hangul / Katakana")
    (vimp-test-buffer
      "한글カタカ[ナ]"
      ("ge")
      "한[글]カタカナ"))
  (ert-info ("Hangul / half-width Katakana")
    (vimp-test-buffer
      "한글ｶﾀｶ[ﾅ]"
      ("ge")
      "한[글]ｶﾀｶﾅ"))
  (ert-info ("Hangul / full-width alphabet")
    (vimp-test-buffer
      "한글ＡＢ[Ｃ]"
      ("ge")
      "한[글]ＡＢＣ"))
  (ert-info ("Hangul / full-width numeric")
    (vimp-test-buffer
      "한글１２[３]"
      ("ge")
      "한[글]１２３")))

(ert-deftest vimp-test-forward-paragraph ()
  "Test `vimp-forward-paragraph'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "[A]bove some line

Below some empty line"
      ("}")
      "Above some line
\[]
Below some empty line"))
  (ert-info ("With count")
    (vimp-test-buffer
      "[A]bove some line

Below some empty line"
      ("2}")
      "Above some line

Below some empty lin[e]"))
  (ert-info ("End of buffer")
    (vimp-test-buffer
      "[B]elow some empty line"
      ("100}")
      "Below some empty lin[e]"
      (should-error (execute-kbd-macro "}"))
      (should-error (execute-kbd-macro "42}"))))
  (ert-info ("End of buffer with newline")
    (vimp-test-buffer
      "[B]elow some empty line\n\n"
      ("100}")
      "Below some empty line\n\n[]"
      (should-error (execute-kbd-macro "}"))
      (should-error (execute-kbd-macro "42}")))))

(ert-deftest vimp-test-backward-paragraph ()
  "Test `vimp-backward-paragraph'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "Above some line

Below some empty lin[e]"
      ("{")
      "Above some line
\[]
Below some empty line"))
  (ert-info ("With count")
    (vimp-test-buffer
      "Above some line

Below some empty lin[e]"
      ("2{")
      "[A]bove some line

Below some empty line"))
  (ert-info ("Beginning of buffer")
    (vimp-test-buffer
      "Above some line

Below some empty lin[e]"
      ("100{")
      "[A]bove some line

Below some empty line"
      (should-error (execute-kbd-macro "{"))
      (should-error (execute-kbd-macro "42{"))))
  (ert-info ("Beginning of buffer with newlines")
    (vimp-test-buffer
      "\n\nAbove some line

Below some empty lin[e]"
      ("100{")
      "[]\n\nAbove some line

Below some empty line"
      (should-error (execute-kbd-macro "{"))
      (should-error (execute-kbd-macro "42{")))))

(ert-deftest vimp-test-forward-sentence ()
  "Test `vimp-forward-sentence'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      (")")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  [I]f you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      (")")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      (")")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

\[B]elow some empty line."))
  (ert-info ("With count")
    (vimp-test-buffer
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      ("2)")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      ("2)")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line[.]"))
  (ert-info ("End of buffer")
    (vimp-test-buffer
      "[B]elow some empty line."
      ("100)")
      "Below some empty line[.]"
      (should-error (execute-kbd-macro ")"))
      (should-error (execute-kbd-macro "42)"))))
  (ert-info ("End of buffer with newline")
    (vimp-test-buffer
      "[B]elow some empty line.\n\n"
      (")")
      "Below some empty line.\n[\n]"
      (")")
      "Below some empty line.\n\n[]"
      (should-error (execute-kbd-macro ")"))
      (should-error (execute-kbd-macro "42)")))))

(ert-deftest vimp-test-backward-sentence ()
  "Test `vimp-backward-sentence'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line[.]"
      ("(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

\[B]elow some empty line."
      ("(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      ("(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  [I]f you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."
      ("(")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line[.]"
      ("2(")
      ";; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.
\[]
Below some empty line."
      ("2(")
      "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.  If you want to create a file,
;; visit that file with C-x C-f.

Below some empty line."))
  (ert-info ("Beginning of buffer")
    (vimp-test-buffer
      ";; This buffer is for notes you don't want to save[.]"
      ("100(")
      "[;]; This buffer is for notes you don't want to save."
      (should-error (execute-kbd-macro "("))
      (should-error (execute-kbd-macro "42("))))
  (ert-info ("Beginning of buffer with newlines")
    (vimp-test-buffer
      "\n\n;; This buffer is for notes you don't want to save[.]"
      ("100(")
      "[]\n\n;; This buffer is for notes you don't want to save."
      (should-error (execute-kbd-macro "("))
      (should-error (execute-kbd-macro "42(")))))

(ert-deftest vimp-test-find-char ()
  "Test `vimp-find-char'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("fT")
      ";; [T]his buffer is for notes."))
  (ert-info ("With count")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("2fe")
      ";; This buffer is for not[e]s."))
  (ert-info ("Repeat")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("fe;")
      ";; This buffer is for not[e]s."))
  (ert-info ("Repeat backward")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("2fe,")
      ";; This buff[e]r is for notes."))
  (ert-info ("No match")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "fL"))))
  (ert-info ("End of line")
    (let ((vimp-cross-lines t))
      (vimp-test-buffer
        "[;]; This buffer is for notes,
;; and for Lisp evaluation."
        ("fL")
        ";; This buffer is for notes,
;; and for [L]isp evaluation."))))

(ert-deftest vimp-test-find-char-backward ()
  "Test `vimp-find-char-backward'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("FT")
      ";; [T]his buffer is for notes."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("2Fe")
      ";; This buff[e]r is for notes."))
  (ert-info ("Repeat")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("Fe;")
      ";; This buff[e]r is for notes."))
  (ert-info ("Repeat backward")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("2Fe,")
      ";; This buffer is for not[e]s."))
  (ert-info ("No match")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "FL"))))
  (ert-info ("End of line")
    (let ((vimp-cross-lines t))
      (vimp-test-buffer
        ";; This buffer is for notes,
;; and for Lisp evaluation[.]"
        ("FT")
        ";; [T]his buffer is for notes,
;; and for Lisp evaluation."))))

(ert-deftest vimp-test-find-char-to ()
  "Test `vimp-find-char-to'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("tT")
      ";;[ ]This buffer is for notes."))
  (ert-info ("With count")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("2te")
      ";; This buffer is for no[t]es."))
  (ert-info ("Repeat")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("tel;")
      ";; This buffer is for no[t]es."))
  (ert-info ("Repeat backward")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      ("2te,")
      ";; This buffe[r] is for notes."))
  (ert-info ("Repeat should skip adjacent character")
    (let ((vimp-repeat-find-to-skip-next t))
      (vimp-test-buffer
        "[a]aaxaaaxaaaxaaa"
        ("tx;")
        "aaaxaa[a]xaaaxaaa"
        (";")
        "aaaxaaaxaa[a]xaaa"
        (",")
        "aaaxaaax[a]aaxaaa"
        (",")
        "aaax[a]aaxaaaxaaa")))
  (ert-info ("Repeat should NOT skip adjacent character")
    (let ((vimp-repeat-find-to-skip-next nil))
      (vimp-test-buffer
        "[a]aaxaaaxaaaxaaa"
        ("tx;")
        "aa[a]xaaaxaaaxaaa")))
  (ert-info ("No match")
    (vimp-test-buffer
      "[;]; This buffer is for notes."
      (should-error (execute-kbd-macro "tL"))))
  (ert-info ("End of line")
    (let ((vimp-cross-lines t))
      (vimp-test-buffer
        "[;]; This buffer is for notes,
;; and for Lisp evaluation."
        ("tL")
        ";; This buffer is for notes,
;; and for[ ]Lisp evaluation."))))

(ert-deftest vimp-test-find-char-to-backward ()
  "Test `vimp-find-char-to-backward'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("TT")
      ";; T[h]is buffer is for notes."))
  (ert-info ("With count")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("2Te")
      ";; This buffe[r] is for notes."))
  (ert-info ("Repeat")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("Teh;")
      ";; This buffe[r] is for notes."))
  (ert-info ("Repeat backward")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      ("2Te,")
      ";; This buffer is for no[t]es."))
  (ert-info ("Repeat should skip adjacent character")
    (let ((vimp-repeat-find-to-skip-next t))
      (vimp-test-buffer
        "aaaxaaaxaaaxaa[a]"
        ("Tx;")
        "aaaxaaax[a]aaxaaa"
        (";")
        "aaax[a]aaxaaaxaaa"
        (",")
        "aaaxaa[a]xaaaxaaa"
        (",")
        "aaaxaaaxaa[a]xaaa")))
  (ert-info ("Repeat should NOT skip adjacent character")
    (let ((vimp-repeat-find-to-skip-next nil))
      (vimp-test-buffer
        "aaaxaaaxaaaxaa[a]"
        ("Tx;")
        "aaaxaaaxaaax[a]aa")))
  (ert-info ("No match")
    (vimp-test-buffer
      ";; This buffer is for notes[.]"
      (should-error (execute-kbd-macro "TL"))))
  (ert-info ("End of line")
    (let ((vimp-cross-lines t))
      (vimp-test-buffer
        ";; This buffer is for notes,
;; and for Lisp evaluation[.]"
        ("TT")
        ";; T[h]is buffer is for notes,
;; and for Lisp evaluation."))))

(ert-deftest vimp-test-jump-item ()
  "Test `vimp-jump-item'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "int main[(]int argc, char** argv)"
      ("%")
      "int main(int argc, char** argv[)]"
      ("%")
      "int main[(]int argc, char** argv)"))
  (ert-info ("Before parenthesis")
    (vimp-test-buffer
      "[i]nt main(int argc, char** argv)"
      ("%")
      "int main(int argc, char** argv[)]"
      ("5h")
      "int main(int argc, char**[ ]argv)"
      ("%")
      "int main[(]int argc, char** argv)"))
  (ert-info ("Over several lines")
    (vimp-test-buffer
      "int main(int argc, char** argv)
\[{]
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
}"
      ("%")
      "int main(int argc, char** argv)
{
  printf(\"Hello world\\n\");
  return EXIT_SUCCESS;
\[}]"))
  (ert-info ("On line without parenthesis")
    (vimp-test-buffer
      "[#]include <stdio.h>"
      (should-error (execute-kbd-macro "%"))))
  (ert-info ("Before unmatched opening parenthesies")
    (vimp-test-buffer
      "x[x]xx ( yyyyy () zzzz"
      (should-error (execute-kbd-macro "%"))
      "x[x]xx ( yyyyy () zzzz"))
  (ert-info ("Before unmatched closing parenthesies")
    (vimp-test-buffer
      "x[x]xx ) yyyyy () zzzz"
      (should-error (execute-kbd-macro "%"))
      "x[x]xx ) yyyyy () zzzz"))

  (ert-info ("At the end of the line")
    (vimp-test-buffer
      "[p]ublic void foo(String bar) {\n   blabla;\n}\n"
      ("v$%")
      "public void foo(String bar) {\n   blabla;\n[}]\n")))

(ert-deftest vimp-test-unmatched-paren ()
  "Test `vimp-previous-open-paren' and `vimp-next-close-paren'"
  :tags '(vimp motion)
  (ert-info ("Simple")
    (vimp-test-buffer
      "foo ( { ( [b]ar ) baz } )"
      ("[(")
      "foo ( { [(] bar ) baz } )"
      ("])")
      "foo ( { ( bar [)] baz } )"
      ("[(")
      "foo ( { [(] bar ) baz } )"
      ("[(")
      "foo [(] { ( bar ) baz } )"
      ("f)])")
      "foo ( { ( bar ) baz } [)]"))
  (ert-info ("With count")
    (vimp-test-buffer
      "foo ( { ( [b]ar ) baz } )"
      ("2[(")
      "foo [(] { ( bar ) baz } )")
    (vimp-test-buffer
      "foo ( { ( [b]ar ) baz } )"
      ("2])")
      "foo ( { ( bar ) baz } [)]")))

;;; Text objects

(ert-deftest vimp-test-text-object ()
  "Test `vimp-define-text-object'"
  :tags '(vimp text-object)
  (let ((object (vimp-define-text-object nil (count &optional beg end type)
                  (let ((sel (and beg end (vimp-range beg end))))
                    (when (and sel (> count 0)) (forward-char 1))
                    (let ((range (if (< count 0)
                                     (list (- (point) 3) (point))
                                   (list (point) (+ (point) 3)))))
                      (if sel
                          (vimp-range-union range sel)
                        range))))))
    (ert-info ("Select three characters after point")
      (vimp-test-buffer
        :state operator
        ";; [T]his buffer is for notes."
        (should (equal (funcall object 1) '(4 7 inclusive)))))
    (ert-info ("Select three characters before point")
      (vimp-test-buffer
        :state operator
        ";; [T]his buffer is for notes."
        (should (equal (funcall object -1) '(1 4 inclusive)))))
    (ert-info ("Select three characters after selection")
      (vimp-test-buffer
        ";; <Thi[s]> buffer is for notes."
        (call-interactively object)
        ";; <This b[u]>ffer is for notes."))
    (ert-info ("Select three characters before selection")
      (vimp-test-buffer
        ";; <[T]his> buffer is for notes."
        (call-interactively object)
        "<[;]; This> buffer is for notes."))
    (ert-info ("Delete three characters after point")
      (vimp-test-buffer
        "[;]; This buffer is for notes."
        (define-key vimp-operator-state-local-map "io" object)
        ("dio")
        "[T]his buffer is for notes."))))

(ert-deftest vimp-test-word-objects ()
  "Test `vimp-inner-word' and `vimp-a-word'"
  :tags '(vimp text-object)
  (ert-info ("Select a word")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("viw")
      ";; <Thi[s]> buffer is for notes.")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("vaw")
      ";; <This[ ]>buffer is for notes.")
    (vimp-test-buffer
      ";; Thi[s] buffer is for notes."
      ("viw")
      ";; <Thi[s]> buffer is for notes.")
    (vimp-test-buffer
      ";; Thi[s] buffer is for notes."
      ("vaw")
      ";; <This[ ]>buffer is for notes."))
  (ert-info ("Select two words")
    (ert-info ("Include whitespace on this side")
      (vimp-test-buffer
        ";;< Thi[s]> buffer is for notes."
        ("aw")
        ";;< This buffe[r]> is for notes.")
      (vimp-test-buffer
        ";; This <[b]uffer >is for notes."
        ("aw")
        ";; <[T]his buffer >is for notes."))
    (ert-info ("Include whitespace on the other side")
      (vimp-test-buffer
        ";; <This[ ]>buffer is for notes."
        ("aw")
        ";; <This buffer[ ]>is for notes.")
      (vimp-test-buffer
        ";; This<[ ]buffer> is for notes."
        ("aw")
        ";;<[ ]This buffer> is for notes.")))
  (ert-info ("select first visual word")
    (vimp-test-buffer
      "([a])"
      ("viw")
      "(<[a]>)")))

(ert-deftest vimp-test-word-objects-cjk ()
  "Test `vimp-inner-word' and `vimp-a-word' on CJK words"
  :tags '(vimp text-object cjk)
  (ert-info ("Select a word")
    (vimp-test-buffer
      "[a]bcd1234"
      ("viw")
      "<abcd123[4]>")
    (vimp-test-buffer
      "[a]bcd1234"
      ("vaw")
      "<abcd123[4]>")
    (vimp-test-buffer
      "[a]bcd漢字"
      ("viw")
      "<abc[d]>漢字")
    (vimp-test-buffer
      "[a]bcd漢字"
      ("vaw")
      "<abc[d]>漢字")
    (vimp-test-buffer
      "[a]bcdひらがな"
      ("viw")
      "<abc[d]>ひらがな")
    (vimp-test-buffer
      "[a]bcdひらがな"
      ("vaw")
      "<abc[d]>ひらがな")
    (vimp-test-buffer
      "[a]bcdカタカナ"
      ("viw")
      "<abc[d]>カタカナ")
    (vimp-test-buffer
      "[a]bcdカタカナ"
      ("vaw")
      "<abc[d]>カタカナ")
    (vimp-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("viw")
      "<abcdｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[a]bcdｶﾀｶﾅ"
      ("vaw")
      "<abcdｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[a]bcdＡＢＣ"
      ("viw")
      "<abcdＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[a]bcdＡＢＣ"
      ("vaw")
      "<abcdＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[a]bcd１２３"
      ("viw")
      "<abcd１２[３]>")
    (vimp-test-buffer
      "[a]bcd１２３"
      ("vaw")
      "<abcd１２[３]>")
    (vimp-test-buffer
      "[a]bcd한글"
      ("viw")
      "<abc[d]>한글")
    (vimp-test-buffer
      "[a]bcd한글"
      ("vaw")
      "<abc[d]>한글")
    (vimp-test-buffer
      "[1]234abcd"
      ("viw")
      "<1234abc[d]>")
    (vimp-test-buffer
      "[1]234abcd"
      ("vaw")
      "<1234abc[d]>")
    (vimp-test-buffer
      "[1]234漢字"
      ("viw")
      "<123[4]>漢字")
    (vimp-test-buffer
      "[1]234漢字"
      ("vaw")
      "<123[4]>漢字")
    (vimp-test-buffer
      "[1]234ひらがな"
      ("viw")
      "<123[4]>ひらがな")
    (vimp-test-buffer
      "[1]234ひらがな"
      ("vaw")
      "<123[4]>ひらがな")
    (vimp-test-buffer
      "[1]234カタカナ"
      ("viw")
      "<123[4]>カタカナ")
    (vimp-test-buffer
      "[1]234カタカナ"
      ("vaw")
      "<123[4]>カタカナ")
    (vimp-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("viw")
      "<1234ｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[1]234ｶﾀｶﾅ"
      ("vaw")
      "<1234ｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[1]234ＡＢＣ"
      ("viw")
      "<1234ＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[1]234ＡＢＣ"
      ("vaw")
      "<1234ＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[1]234１２３"
      ("viw")
      "<1234１２[３]>")
    (vimp-test-buffer
      "[1]234１２３"
      ("vaw")
      "<1234１２[３]>")
    (vimp-test-buffer
      "[1]234한글"
      ("viw")
      "<123[4]>한글")
    (vimp-test-buffer
      "[1]234한글"
      ("vaw")
      "<123[4]>한글")
    (vimp-test-buffer
      "[漢]字abcd"
      ("viw")
      "<漢[字]>abcd")
    (vimp-test-buffer
      "[漢]字abcd"
      ("vaw")
      "<漢[字]>abcd")
    (vimp-test-buffer
      "[漢]字1234"
      ("viw")
      "<漢[字]>1234")
    (vimp-test-buffer
      "[漢]字1234"
      ("vaw")
      "<漢[字]>1234")
    (vimp-test-buffer
      "[漢]字ひらがな"
      ("viw")
      "<漢[字]>ひらがな")
    (vimp-test-buffer
      "[漢]字ひらがな"
      ("vaw")
      "<漢[字]>ひらがな")
    (vimp-test-buffer
      "[漢]字カタカナ"
      ("viw")
      "<漢[字]>カタカナ")
    (vimp-test-buffer
      "[漢]字カタカナ"
      ("vaw")
      "<漢[字]>カタカナ")
    (vimp-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("viw")
      "<漢[字]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[漢]字ｶﾀｶﾅ"
      ("vaw")
      "<漢[字]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[漢]字ＡＢＣ"
      ("viw")
      "<漢[字]>ＡＢＣ")
    (vimp-test-buffer
      "[漢]字ＡＢＣ"
      ("vaw")
      "<漢[字]>ＡＢＣ")
    (vimp-test-buffer
      "[漢]字１２３"
      ("viw")
      "<漢[字]>１２３")
    (vimp-test-buffer
      "[漢]字１２３"
      ("vaw")
      "<漢[字]>１２３")
    (vimp-test-buffer
      "[漢]字한글"
      ("viw")
      "<漢[字]>한글")
    (vimp-test-buffer
      "[漢]字한글"
      ("vaw")
      "<漢[字]>한글")
    (vimp-test-buffer
      "[ひ]らがなabcd"
      ("viw")
      "<ひらが[な]>abcd")
    (vimp-test-buffer
      "[ひ]らがなabcd"
      ("vaw")
      "<ひらが[な]>abcd")
    (vimp-test-buffer
      "[ひ]らがな1234"
      ("viw")
      "<ひらが[な]>1234")
    (vimp-test-buffer
      "[ひ]らがな1234"
      ("vaw")
      "<ひらが[な]>1234")
    (vimp-test-buffer
      "[ひ]らがな漢字"
      ("viw")
      "<ひらが[な]>漢字")
    (vimp-test-buffer
      "[ひ]らがな漢字"
      ("vaw")
      "<ひらが[な]>漢字")
    (vimp-test-buffer
      "[ひ]らがなカタカナ"
      ("viw")
      "<ひらが[な]>カタカナ")
    (vimp-test-buffer
      "[ひ]らがなカタカナ"
      ("vaw")
      "<ひらが[な]>カタカナ")
    (vimp-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("viw")
      "<ひらが[な]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[ひ]らがなｶﾀｶﾅ"
      ("vaw")
      "<ひらが[な]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[ひ]らがなＡＢＣ"
      ("viw")
      "<ひらが[な]>ＡＢＣ")
    (vimp-test-buffer
      "[ひ]らがなＡＢＣ"
      ("vaw")
      "<ひらが[な]>ＡＢＣ")
    (vimp-test-buffer
      "[ひ]らがな１２３"
      ("viw")
      "<ひらが[な]>１２３")
    (vimp-test-buffer
      "[ひ]らがな１２３"
      ("vaw")
      "<ひらが[な]>１２３")
    (vimp-test-buffer
      "[ひ]らがな한글"
      ("viw")
      "<ひらが[な]>한글")
    (vimp-test-buffer
      "[ひ]らがな한글"
      ("vaw")
      "<ひらが[な]>한글")
    (vimp-test-buffer
      "[カ]タカナabcd"
      ("viw")
      "<カタカ[ナ]>abcd")
    (vimp-test-buffer
      "[カ]タカナabcd"
      ("vaw")
      "<カタカ[ナ]>abcd")
    (vimp-test-buffer
      "[カ]タカナ1234"
      ("viw")
      "<カタカ[ナ]>1234")
    (vimp-test-buffer
      "[カ]タカナ1234"
      ("vaw")
      "<カタカ[ナ]>1234")
    (vimp-test-buffer
      "[カ]タカナ漢字"
      ("viw")
      "<カタカ[ナ]>漢字")
    (vimp-test-buffer
      "[カ]タカナ漢字"
      ("vaw")
      "<カタカ[ナ]>漢字")
    (vimp-test-buffer
      "[カ]タカナひらがな"
      ("viw")
      "<カタカ[ナ]>ひらがな")
    (vimp-test-buffer
      "[カ]タカナひらがな"
      ("vaw")
      "<カタカ[ナ]>ひらがな")
    (vimp-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("viw")
      "<カタカ[ナ]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[カ]タカナｶﾀｶﾅ"
      ("vaw")
      "<カタカ[ナ]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[カ]タカナＡＢＣ"
      ("viw")
      "<カタカ[ナ]>ＡＢＣ")
    (vimp-test-buffer
      "[カ]タカナＡＢＣ"
      ("vaw")
      "<カタカ[ナ]>ＡＢＣ")
    (vimp-test-buffer
      "[カ]タカナ１２３"
      ("viw")
      "<カタカ[ナ]>１２３")
    (vimp-test-buffer
      "[カ]タカナ１２３"
      ("vaw")
      "<カタカ[ナ]>１２３")
    (vimp-test-buffer
      "[カ]タカナ한글"
      ("viw")
      "<カタカ[ナ]>한글")
    (vimp-test-buffer
      "[カ]タカナ한글"
      ("vaw")
      "<カタカ[ナ]>한글")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("viw")
      "<ｶﾀｶﾅabc[d]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅabcd"
      ("vaw")
      "<ｶﾀｶﾅabc[d]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("viw")
      "<ｶﾀｶﾅ123[4]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ1234"
      ("vaw")
      "<ｶﾀｶﾅ123[4]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("viw")
      "<ｶﾀｶ[ﾅ]>漢字")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ漢字"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>漢字")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("viw")
      "<ｶﾀｶ[ﾅ]>ひらがな")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅひらがな"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>ひらがな")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("viw")
      "<ｶﾀｶ[ﾅ]>カタカナ")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅカタカナ"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>カタカナ")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("viw")
      "<ｶﾀｶﾅＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅＡＢＣ"
      ("vaw")
      "<ｶﾀｶﾅＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("viw")
      "<ｶﾀｶﾅ１２[３]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ１２３"
      ("vaw")
      "<ｶﾀｶﾅ１２[３]>")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("viw")
      "<ｶﾀｶ[ﾅ]>한글")
    (vimp-test-buffer
      "[ｶ]ﾀｶﾅ한글"
      ("vaw")
      "<ｶﾀｶ[ﾅ]>한글")
    (vimp-test-buffer
      "[Ａ]ＢＣabcd"
      ("viw")
      "<ＡＢＣabc[d]>")
    (vimp-test-buffer
      "[Ａ]ＢＣabcd"
      ("vaw")
      "<ＡＢＣabc[d]>")
    (vimp-test-buffer
      "[Ａ]ＢＣ1234"
      ("viw")
      "<ＡＢＣ123[4]>")
    (vimp-test-buffer
      "[Ａ]ＢＣ1234"
      ("vaw")
      "<ＡＢＣ123[4]>")
    (vimp-test-buffer
      "[Ａ]ＢＣ漢字"
      ("viw")
      "<ＡＢ[Ｃ]>漢字")
    (vimp-test-buffer
      "[Ａ]ＢＣ漢字"
      ("vaw")
      "<ＡＢ[Ｃ]>漢字")
    (vimp-test-buffer
      "[Ａ]ＢＣひらがな"
      ("viw")
      "<ＡＢ[Ｃ]>ひらがな")
    (vimp-test-buffer
      "[Ａ]ＢＣひらがな"
      ("vaw")
      "<ＡＢ[Ｃ]>ひらがな")
    (vimp-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("viw")
      "<ＡＢ[Ｃ]>カタカナ")
    (vimp-test-buffer
      "[Ａ]ＢＣカタカナ"
      ("vaw")
      "<ＡＢ[Ｃ]>カタカナ")
    (vimp-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("viw")
      "<ＡＢＣｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[Ａ]ＢＣｶﾀｶﾅ"
      ("vaw")
      "<ＡＢＣｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[Ａ]ＢＣ１２３"
      ("viw")
      "<ＡＢＣ１２[３]>")
    (vimp-test-buffer
      "[Ａ]ＢＣ１２３"
      ("vaw")
      "<ＡＢＣ１２[３]>")
    (vimp-test-buffer
      "[Ａ]ＢＣ한글"
      ("viw")
      "<ＡＢ[Ｃ]>한글")
    (vimp-test-buffer
      "[Ａ]ＢＣ한글"
      ("vaw")
      "<ＡＢ[Ｃ]>한글")
    (vimp-test-buffer
      "[１]２３abcd"
      ("viw")
      "<１２３abc[d]>")
    (vimp-test-buffer
      "[１]２３abcd"
      ("vaw")
      "<１２３abc[d]>")
    (vimp-test-buffer
      "[１]２３1234"
      ("viw")
      "<１２３123[4]>")
    (vimp-test-buffer
      "[１]２３1234"
      ("vaw")
      "<１２３123[4]>")
    (vimp-test-buffer
      "[１]２３漢字"
      ("viw")
      "<１２[３]>漢字")
    (vimp-test-buffer
      "[１]２３漢字"
      ("vaw")
      "<１２[３]>漢字")
    (vimp-test-buffer
      "[１]２３ひらがな"
      ("viw")
      "<１２[３]>ひらがな")
    (vimp-test-buffer
      "[１]２３ひらがな"
      ("vaw")
      "<１２[３]>ひらがな")
    (vimp-test-buffer
      "[１]２３カタカナ"
      ("viw")
      "<１２[３]>カタカナ")
    (vimp-test-buffer
      "[１]２３カタカナ"
      ("vaw")
      "<１２[３]>カタカナ")
    (vimp-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("viw")
      "<１２３ｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[１]２３ｶﾀｶﾅ"
      ("vaw")
      "<１２３ｶﾀｶ[ﾅ]>")
    (vimp-test-buffer
      "[１]２３ＡＢＣ"
      ("viw")
      "<１２３ＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[１]２３ＡＢＣ"
      ("vaw")
      "<１２３ＡＢ[Ｃ]>")
    (vimp-test-buffer
      "[１]２３한글"
      ("viw")
      "<１２[３]>한글")
    (vimp-test-buffer
      "[１]２３한글"
      ("vaw")
      "<１２[３]>한글")
    (vimp-test-buffer
      "[한]글abcd"
      ("viw")
      "<한[글]>abcd")
    (vimp-test-buffer
      "[한]글abcd"
      ("vaw")
      "<한[글]>abcd")
    (vimp-test-buffer
      "[한]글1234"
      ("viw")
      "<한[글]>1234")
    (vimp-test-buffer
      "[한]글1234"
      ("vaw")
      "<한[글]>1234")
    (vimp-test-buffer
      "[한]글漢字"
      ("viw")
      "<한[글]>漢字")
    (vimp-test-buffer
      "[한]글漢字"
      ("vaw")
      "<한[글]>漢字")
    (vimp-test-buffer
      "[한]글ひらがな"
      ("viw")
      "<한[글]>ひらがな")
    (vimp-test-buffer
      "[한]글ひらがな"
      ("vaw")
      "<한[글]>ひらがな")
    (vimp-test-buffer
      "[한]글カタカナ"
      ("viw")
      "<한[글]>カタカナ")
    (vimp-test-buffer
      "[한]글カタカナ"
      ("vaw")
      "<한[글]>カタカナ")
    (vimp-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("viw")
      "<한[글]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[한]글ｶﾀｶﾅ"
      ("vaw")
      "<한[글]>ｶﾀｶﾅ")
    (vimp-test-buffer
      "[한]글ＡＢＣ"
      ("viw")
      "<한[글]>ＡＢＣ")
    (vimp-test-buffer
      "[한]글ＡＢＣ"
      ("vaw")
      "<한[글]>ＡＢＣ")
    (vimp-test-buffer
      "[한]글１２３"
      ("viw")
      "<한[글]>１２３")
    (vimp-test-buffer
      "[한]글１２３"
      ("vaw")
      "<한[글]>１２３")))

(ert-deftest vimp-test-paragraph-objects ()
  "Test `vimp-inner-paragraph' and `vimp-a-paragraph'"
  :tags '(vimp text-object)
  (ert-info ("Select a paragraph with point at beginning")
    (vimp-test-buffer
      "[;]; This buffer is for notes,
;; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vap")
      "<;; This buffer is for notes,
;; and for Lisp evaluation.
\[]\n>\
;; This buffer is for notes,
;; and for Lisp evaluation."))
  (ert-info ("Select a paragraph with point at last line")
    (vimp-test-buffer
      ";; This buffer is for notes,
\[;]; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vap")
      "<;; This buffer is for notes,
;; and for Lisp evaluation.
\[]\n>\
;; This buffer is for notes,
;; and for Lisp evaluation."))
  (ert-info ("Select a paragraph with point after paragraph")
    (vimp-test-buffer
      ";; This buffer is for notes,
;; and for Lisp evaluation.
\[]
;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vap")
      ";; This buffer is for notes,
;; and for Lisp evaluation.
<
;; This buffer is for notes,
;; and for Lisp evaluation[.]>"))
  (ert-info ("Select inner paragraph")
    (vimp-test-buffer
      "[;]; This buffer is for notes,
;; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vip")
      "<;; This buffer is for notes,
;; and for Lisp evaluation.[]
>
;; This buffer is for notes,
;; and for Lisp evaluation.")
    (vimp-test-buffer
      ";; This buffer is for notes,
\[;]; and for Lisp evaluation.

;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vip")
      "<;; This buffer is for notes,
;; and for Lisp evaluation.[]
>
;; This buffer is for notes,
;; and for Lisp evaluation.")
    (vimp-test-buffer
      ";; This buffer is for notes,
;; and for Lisp evaluation.
\[]
;; This buffer is for notes,
;; and for Lisp evaluation."
      ("vip")
      ";; This buffer is for notes,
;; and for Lisp evaluation.
<
;; This buffer is for notes,
;; and for Lisp evaluation[.]>")))

(ert-deftest vimp-test-quote-objects ()
  "Test `vimp-inner-single-quote' and `vimp-a-single-quote'"
  :tags '(vimp text-object)
  (ert-info ("Select text inside of '...'")
    (vimp-test-buffer
      "This is 'a [t]est' for quote objects."
      ("vi'")
      "This is '<a tes[t]>' for quote objects.")
    (vimp-test-buffer
      "This is \"a '[t]est'\" for quote objects."
      ("vi'")
      "This is \"a '<tes[t]>'\" for quote objects."))
  (ert-info ("Select text including enclosing quotes")
    (vimp-test-buffer
      "This is 'a [t]est' for quote objects."
      ("v2i'")
      "This is <'a test[']> for quote objects."))
  (ert-info ("Select text including enclosing quotes and following space")
    (vimp-test-buffer
      "This is 'a [t]est' for quote objects."
      ("va'")
      "This is <'a test'[ ]>for quote objects."))
  (ert-info ("Select text including enclosing quotes and previous space")
    (vimp-test-buffer
      "This is 'a [t]est'. For quote objects."
      ("va'")
      "This is< 'a test[']>. For quote objects."))
  (ert-info ("Select text on opening quote")
    (vimp-test-buffer
      "This is [\"]a test\". For \"quote\" objects."
      (emacs-lisp-mode)
      ("va\"")
      "This is< \"a test[\"]>. For \"quote\" objects."))
  (ert-info ("Select text on closing quote")
    (vimp-test-buffer
      "This is \"a test[\"]. For \"quote\" objects."
      (emacs-lisp-mode)
      ("va\"")
      "This is< \"a test[\"]>. For \"quote\" objects."))
  (ert-info ("Delete text from outside")
    (vimp-test-buffer
      "Th[i]s is \"a test\". For \"quote\" objects."
      (emacs-lisp-mode)
      ("da\"")
      "This is[.] For \"quote\" objects."))
  (ert-info ("Operator on empty quotes")
    (vimp-test-buffer
      "This is [a]n \"\" empty quote"
      (emacs-lisp-mode)
      ("ci\"XXX" [escape])
      "This is an \"XX[X]\" empty quote")))

(ert-deftest vimp-test-paren-objects ()
  "Test `vimp-inner-paren', etc."
  :tags '(vimp text-object)
  (ert-info ("Select inner text")
    (vimp-test-buffer
      "[(]aaa)"
      (emacs-lisp-mode) ; syntax
      ("vi(")
      "(<aa[a]>)")
    (vimp-test-buffer
      "(aaa[)]"
      (emacs-lisp-mode)
      ("vi(")
      "(<aa[a]>)")
    (ert-info ("Next to outer delimiter")
      (vimp-test-buffer
        "([(]aaa))"
        (emacs-lisp-mode)
        ("vi(")
        "((<aa[a]>))")
      (vimp-test-buffer
        "((aaa[)])"
        (emacs-lisp-mode)
        ("vi(")
        "((<aa[a]>))")))
  (ert-info ("Select double inner parentheses")
    (vimp-test-buffer
      "([(]word))"
      ("dib")
      "(())")
    (vimp-test-buffer
      "[(](word))"
      ("dib")
      "()")
    (vimp-test-buffer
      "((word[)])"
      ("dib")
      "(())")
    (vimp-test-buffer
      "((word)[)]"
      ("dib")
      "()"))
  (ert-info ("Select double outer parentheses")
    (vimp-test-buffer
      "a([(]word))b"
      ("dab")
      "a()b")
    (vimp-test-buffer
      "a[(](word))b"
      ("dab")
      "ab")
    (vimp-test-buffer
      "a((word[)])b"
      ("dab")
      "a()b")
    (vimp-test-buffer
      "a((word)[)]b"
      ("dab")
      "ab"))
  (ert-info ("Select parentheses inside strings")
    (vimp-test-buffer
      "(aaa \"b(b[b]b)\" aa)"
      (emacs-lisp-mode)
      ("va(")
      "(aaa \"b<(bbb[)]>\" aa)"))
  (ert-info ("Break out of empty strings")
    (vimp-test-buffer
      "(aaa \"bb[b]b\" aa)"
      (emacs-lisp-mode)
      ("va(")
      "<(aaa \"bbbb\" aa[)]>"))
  (ert-info ("Select inner parentheses around strings")
    (vimp-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vib")
      "((<\"test[\"]>))\n"
      ("ib")
      "(<(\"test\"[)]>)\n")
    (vimp-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vib")
      "( (< \"test\"[ ]>) )\n"
      ("ib")
      "(< ( \"test\" )[ ]>)\n")
    (vimp-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vhhib")
      "((<[\"]test\">))\n"
      ("ib")
      "(<[(]\"test\")>)\n")
    (vimp-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vhhib")
      "( (<[ ]\"test\" >) )\n"
      ("ib")
      "(<[ ]( \"test\" ) >)\n"))
  (ert-info ("Select outer parentheses around strings")
    (vimp-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vab")
      "(<(\"test\"[)]>)\n"
      ("ab")
      "<((\"test\")[)]>\n")
    (vimp-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vab")
      "( <( \"test\" [)]> )\n"
      ("ab")
      "<( ( \"test\" ) [)]>\n")
    (vimp-test-buffer
      "((\"t[e]st\"))\n"
      (emacs-lisp-mode)
      ("vhhab")
      "(<[(]\"test\")>)\n"
      ("ab")
      "<[(](\"test\"))>\n")
    (vimp-test-buffer
      "( ( \"t[e]st\" ) )\n"
      (emacs-lisp-mode)
      ("vhhab")
      "( <[(] \"test\" )> )\n"
      ("ab")
      "<[(] ( \"test\" ) )>\n")
    (vimp-test-buffer
      "(([\"]\"))\n"
      ("dab")
      "([)]\n"))
  (ert-info ("Enlarge to smallest complete surrounding")
    (vimp-test-buffer
      "for (auto i : vector) {
  if (c<ond) {
    do_[s]>omething();
  }
}"
      ("i}")
      "for (auto i : vector) {<
  if (cond) {
    do_something();
  }[\n]>}")))

(ert-deftest vimp-test-forces-linewise-text-objects ()
  "Test `vimp-text-object-change-visual-type' option."
  :tags '(vimp text-object)
  (let ((vimp-text-object-change-visual-type t))
    (ert-info ("Change visual type")
      (vimp-test-buffer
        "  function(opts) {
    this.var1 = something();
    [t]his.var2 = something_else();
    return something_nasty();
  }
"
        ("Vi}")
        "  function(opts) {<
    this.var1 = something();
    this.var2 = something_else();
    return something_nasty();
 [ ]>}
"
        (should (eq (vimp-visual-type) 'inclusive)))))
  (let ((vimp-text-object-change-visual-type nil))
    (ert-info ("Change visual type keeping linewise")
      (vimp-test-buffer
        "  function(opts) {
    this.var1 = something();
    [t]his.var2 = something_else();
    return something_nasty();
  }
"
        ("Vi}")
        "  function(opts) {
<    this.var1 = something();
    this.var2 = something_else();
    return something_nasty();\n>  }
"
        (should (eq (vimp-visual-type) 'line)))))
  (let ((vimp-text-object-change-visual-type nil))
    (ert-info ("Linewise outer block")
      (vimp-test-buffer
        "  function(opts) {
    this.var1 = something();
    [t]his.var2 = something_else();
    return something_nasty();
  }
"
        ("Va}")
        "<  function(opts) {
    this.var1 = something();
    this.var2 = something_else();
    return something_nasty();
  }
>"
        (should (eq (vimp-visual-type) 'line)))))
  (ert-info ("Forced motion type should change text object type")
    (vimp-test-buffer
      "for (int i=0; i<10; i++) {
  if ([c]ond) {
    do_something();
  }
}"
      ("dVi}")
      "for (int i=0; i<10; i++) {
\[}]")))

(ert-deftest vimp-test-tag-objects ()
  "Test `vimp-inner-tag', etc."
  :tags '(vimp text-object)
  (ert-info ("Handle nested tags")
    (vimp-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<p><a>f[o]o</a> bar</p>"
      ("vit")
      "<p><a>{fo[o]}</a> bar</p>"))
  (ert-info ("Break out of tags")
    (vimp-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<a[a]a>bbbb</aaa>"
      ("vit")
      "<aaa>{bbb[b]}</aaa>")
    (vimp-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<a[a]a>bbbb</aaa>"
      ("vat")
      "{<aaa>bbbb</aaa[>]}"))
  (ert-info ("Handle quoted strings tags")
    (vimp-test-buffer
      :visual-start "{"
      :visual-end "}"
      "<html>
<body>
<div id=\"content\">
\[ ]
<p>
UPDATE
</p>
<p>
test hello <a href=\"/deed.zh\">Creative Commons</a>
</p>
</div>
</body>
</html>
"
      ("vit")
      "<html>
<body>
<div id=\"content\">{\n \n<p>
UPDATE
</p>
<p>
test hello <a href=\"/deed.zh\">Creative Commons</a>
</p>[\n]}</div>
</body>
</html>
"

      )))

;;; Visual state

(defun vimp-test-visual-select (selection &optional mark point)
  "Verify that TYPE is selected correctly"
  (let ((type (vimp-visual-type selection)))
    (vimp-visual-make-selection mark point type)
    (ert-info ("Activate region unless SELECTION is `block'")
      (cond
       ((eq selection 'block)
        (should (mark t))
        (should-not (region-active-p))
        (should-not transient-mark-mode))
       (t
        (should (mark))
        (should (region-active-p)))))
    (ert-info ("Refresh Visual markers")
      (should (= (vimp-range-beginning (vimp-expand (point) (mark) type))
                 vimp-visual-beginning))
      (should (= (vimp-range-end (vimp-expand (point) (mark) type))
                 vimp-visual-end))
      (should (eq (vimp-visual-type) type))
      (should (eq vimp-visual-direction
                  (if (< (point) (mark)) -1 1))))))

(ert-deftest vimp-test-visual-refresh ()
  "Test `vimp-visual-refresh'"
  :tags '(vimp visual)
  (vimp-test-buffer
    ";; [T]his buffer is for notes."
    (vimp-visual-refresh nil nil 'inclusive)
    (should (= vimp-visual-beginning 4))
    (should (= vimp-visual-end 5)))
  (vimp-test-buffer
    ";; [T]his buffer is for notes."
    (let ((vimp-visual-region-expanded t))
      (vimp-visual-refresh nil nil 'inclusive)
      (should (= vimp-visual-beginning 4))
      (should (= vimp-visual-end 4)))))

(ert-deftest vimp-test-visual-exchange ()
  "Test `exchange-point-and-mark' in Visual character selection"
  :tags '(vimp visual)
  (vimp-test-buffer
    ";; <[T]his> buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("o")
    (should (region-active-p))
    ";; <Thi[s]> buffer is for notes you don't want to save,
;; and for Lisp evaluation."))

(ert-deftest vimp-test-visual-char ()
  "Test Visual character selection"
  :tags '(vimp visual)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    (vimp-test-visual-select 'char)
    ";; <[T]>his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("e")
    ";; <Thi[s]> buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("o")
    ";; <[T]his> buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("d")
    ";; [ ]buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    ("vV")
    "<;; [ ]buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation.")
  (ert-info ("Test `vimp-want-visual-char-semi-exclusive")
    (let ((vimp-want-visual-char-semi-exclusive t))
      (vimp-test-buffer
        "[;]; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; And a third line."
        ("v")
        "<[;]>; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.
;; And a third line."
        ("$")
        "<;; This buffer is for notes you don't want to save,>[
];; and for Lisp evaluation.
;; And a third line."
        ("^jj")
        "<;; This buffer is for notes you don't want to save,
;; and for Lisp evaluation.\n>[;]; And a third line."))))

(ert-deftest vimp-test-visual-line ()
  "Test Visual line selection"
  :tags '(vimp visual)
  (vimp-test-buffer
    ";; [T]his buffer is for notes you don't want to save,
;; and for Lisp evaluation."
    (vimp-test-visual-select 'line)
    "<;; [T]his buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation."
    ("e")
    "<;; Thi[s] buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation."
    ("o")
    "<;; [T]his buffer is for notes you don't want to save,\n>\
;; and for Lisp evaluation."
    ("d")
    "[;]; and for Lisp evaluation."))

(ert-deftest vimp-test-visual-block ()
  "Test Visual block selection"
  :tags '(vimp visual)
  (vimp-test-buffer
    "[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    (vimp-test-visual-select 'block)
    "<[;]>; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer."
    ("jjll")
    "<;; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;;[ ]>then enter the text in that file's own buffer."
    ("O")
    ";; <This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
>[;]; then enter the text in that file's own buffer."
    ("o")
    ";;[ ]<This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
>;; then enter the text in that file's own buffer."
    ("O")
    "<[;]; This buffer is for notes you don't want to save.
;; If you want to create a file, visit that file with C-x C-f,
;; >then enter the text in that file's own buffer."
    ("d")
    "This buffer is for notes you don't want to save.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer."))

(ert-deftest vimp-test-visual-restore ()
  "Test restoring a previous selection"
  :tags '(vimp visual)
  (ert-info ("Start a characterwise selection \
if no previous selection")
    (vimp-test-buffer
      ";; [T]his buffer is for notes."
      ("gv")
      ";; <[T]>his buffer is for notes."))
  (ert-info ("Restore characterwise selection")
    (vimp-test-buffer
      ";; <[T]his> buffer is for notes."
      ([escape] "gv")
      ";; <[T]his> buffer is for notes."))
  (ert-info ("Restore linewise selection")
    (vimp-test-buffer
      :visual line
      "<;; [T]his buffer is for notes.>"
      ([escape] "gv")
      "<;; [T]his buffer is for notes.>"))
  (ert-info ("Restore blockwise selection")
    (vimp-test-buffer
      :visual block
      "<;; This buffer is for notes,
;;[ ]>and for Lisp evaluation."
      ([escape] "gv")
      "<;; This buffer is for notes,
;;[ ]>and for Lisp evaluation.")))

;;; Ex

(ert-deftest vimp-test-ex-parse ()
  "Test `vimp-ex-parse'"
  :tags '(vimp ex)
  (should (equal (vimp-ex-parse "5,2cmd arg")
                 '(vimp-ex-call-command
                   (vimp-ex-range
                    (vimp-ex-line (string-to-number "5") nil)
                    (vimp-ex-line (string-to-number "2") nil))
                   "cmd"
                   "arg")))
  (should (equal (vimp-ex-parse "5,2cmd !arg")
                 '(vimp-ex-call-command
                   (vimp-ex-range
                    (vimp-ex-line (string-to-number "5") nil)
                    (vimp-ex-line (string-to-number "2") nil))
                   "cmd"
                   "!arg")))
  (should (equal (vimp-ex-parse "5,2 arg")
                 '(vimp-ex-call-command
                   (vimp-ex-range
                    (vimp-ex-line (string-to-number "5") nil)
                    (vimp-ex-line (string-to-number "2") nil))
                   "arg"
                   nil))))

(ert-deftest vimp-test-ex-parse-ranges ()
  "Test parsing of ranges"
  :tags '(vimp ex)
  (should (equal (vimp-ex-parse "%" nil 'range)
                 '(vimp-ex-full-range)))
  (should (equal (vimp-ex-parse "5,27" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line (string-to-number "5") nil)
                   (vimp-ex-line (string-to-number "27") nil))))
  (should (equal (vimp-ex-parse "5,$" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line (string-to-number "5") nil)
                   (vimp-ex-line (vimp-ex-last-line) nil))))
  (should (equal (vimp-ex-parse "5,'x" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line (string-to-number "5") nil)
                   (vimp-ex-line (vimp-ex-marker "x") nil))))
  (should (equal (vimp-ex-parse "`x,`y" nil 'range)
                 '(vimp-ex-char-marker-range "x" "y")))
  (should (equal (vimp-ex-parse "5,+" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line (string-to-number "5") nil)
                   (vimp-ex-line
                    nil (+ (vimp-ex-signed-number (intern "+") nil))))))
  (should (equal (vimp-ex-parse "5,-" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line (string-to-number "5") nil)
                   (vimp-ex-line
                    nil (+ (vimp-ex-signed-number (intern "-") nil))))))
  (should (equal (vimp-ex-parse "5,4+2-7-3+10-" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line (string-to-number "5") nil)
                   (vimp-ex-line
                    (string-to-number "4")
                    (+ (vimp-ex-signed-number
                        (intern "+") (string-to-number "2"))
                       (vimp-ex-signed-number
                        (intern "-") (string-to-number "7"))
                       (vimp-ex-signed-number
                        (intern "-") (string-to-number "3"))
                       (vimp-ex-signed-number
                        (intern "+") (string-to-number "10"))
                       (vimp-ex-signed-number (intern "-") nil))))))
  (should (equal (vimp-ex-parse ".-2,4+2-7-3+10-" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line
                    (vimp-ex-current-line)
                    (+ (vimp-ex-signed-number
                        (intern "-") (string-to-number "2"))))
                   (vimp-ex-line
                    (string-to-number "4")
                    (+ (vimp-ex-signed-number
                        (intern "+") (string-to-number "2"))
                       (vimp-ex-signed-number
                        (intern "-") (string-to-number "7"))
                       (vimp-ex-signed-number
                        (intern "-") (string-to-number "3"))
                       (vimp-ex-signed-number
                        (intern "+") (string-to-number "10"))
                       (vimp-ex-signed-number
                        (intern "-") nil))))))
  (should (equal (vimp-ex-parse "'a-2,$-10" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line
                    (vimp-ex-marker "a")
                    (+ (vimp-ex-signed-number
                        (intern "-") (string-to-number "2"))))
                   (vimp-ex-line
                    (vimp-ex-last-line)
                    (+ (vimp-ex-signed-number
                        (intern "-") (string-to-number "10")))))))
  (should (equal (vimp-ex-parse ".+42" nil 'range)
                 '(vimp-ex-range
                   (vimp-ex-line
                    (vimp-ex-current-line)
                    (+ (vimp-ex-signed-number
                        (intern "+") (string-to-number "42"))))
                   nil))))

(ert-deftest vimp-text-ex-search-offset ()
  "Test for addresses like /base//pattern/"
  :tags '(vimp ex)
  (ert-info ("without base")
    (vimp-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd"
      (":/aaa/d")
      "line 1\nbbb\naaa\nccc\nddd"))
  (ert-info ("with base")
    (vimp-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd"
      (":/bbb//aaa/d")
      "line 1\naaa\nbbb\nccc\nddd"))
  (ert-info ("range without base")
    (vimp-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd\nccc\neee\n"
      (":/aaa/;/ccc/d")
      "line 1\nddd\nccc\neee\n"))
  (ert-info ("range with base")
    (vimp-test-buffer
      "[l]ine 1\naaa\nbbb\naaa\nccc\nddd\nccc\neee\n"
      (":/bbb//aaa/;/ddd//ccc/d")
      "line 1\naaa\nbbb\neee\n")))

(ert-deftest vimp-test-ex-goto-line ()
  "Test if :number moves point to a certain line"
  :tags '(vimp ex)
  (ert-info ("Move to line")
    (vimp-test-buffer
      :visual line
      "1\n 2\n [ ]3\n   4\n    5\n"
      (":4" [return])
      "1\n 2\n  3\n   [4]\n    5\n"
      (":2" [return])
      "1\n [2]\n  3\n   4\n    5\n")))

(ert-deftest vimp-test-ex-repeat ()
  "Test :@: command."
  :tags '(vimp ex)
  (vimp-without-display
    (ert-info ("Repeat in current line")
      (vimp-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X/g" [return])
        "[a]XcdXf\nabcdef\nabcdef"
        ("jj:@:" [return])
        "aXcdXf\nabcdef\n[a]XcdXf"))
    (ert-info ("Repeat in specified line")
      (vimp-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X/g" [return])
        "[a]XcdXf\nabcdef\nabcdef"
        (":3@:" [return])
        "aXcdXf\nabcdef\n[a]XcdXf"))
    (ert-info ("Double repeat, first without then with specified line")
      (vimp-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X/" [return])
        "[a]Xcdef\nabcdef\nabcdef"
        ("jj:@:" [return] ":1@:" [return])
        "[a]XcdXf\nabcdef\naXcdef"))))

(ert-deftest vimp-test-ex-repeat2 ()
  "Test @: command."
  :tags '(vimp ex)
  (vimp-without-display
    (ert-info ("Repeat in current line")
      (vimp-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X" [return])
        "[a]Xcdef\nabcdef\nabcdef"
        ("jj@:")
        "aXcdef\nabcdef\n[a]Xcdef"))
    (ert-info ("Repeat with count in current line")
      (vimp-test-buffer
        "[a]bcdef\nabcdef\nabcdef"
        (":s/[be]/X" [return])
        "[a]Xcdef\nabcdef\nabcdef"
        ("jj2@:")
        "aXcdef\nabcdef\n[a]XcdXf"))
    (ert-info ("Do not record dot repeat")
      (vimp-test-buffer
        ""
        ("OAAAAAA" [escape] "^")
        "[A]AAAAA\n"
        (":s/A/X" [return])
        "[X]AAAAA\n"
        ("@:")
        "[X]XAAAA\n"
        (".")
        "AAAAAA\nXXAAAA\n"))))

(ert-deftest vimp-test-ex-visual-char-range ()
  "Test visual character ranges in ex state."
  :tags '(vimp ex visual)
  (vimp-without-display
    (ert-info ("No character range, inclusive")
      (let ((vimp-visual-char 'inclusive)
            vimp-ex-visual-char-range)
        (vimp-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "line 3\nline 4\n")))
    (ert-info ("No character range, exclusive")
      (let ((vimp-visual-char 'inclusive)
            vimp-ex-visual-char-range)
        (vimp-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "line 3\nline 4\n")))
    (ert-info ("Character range, inclusive")
      (let ((vimp-visual-char 'inclusive)
            (vimp-ex-visual-char-range t))
        (vimp-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "li2\nline 3\nline 4\n")))
    (ert-info ("Character range, exclusive")
      (let ((vimp-visual-char 'exclusive)
            (vimp-ex-visual-char-range t))
        (vimp-test-buffer
          "li[n]e 1\nline 2\nline 3\nline 4\n"
          ("vjll:d" [return])
          "li 2\nline 3\nline 4\n")))))

(ert-deftest vimp-test-ex-substitute-replacement ()
  "Test `vimp-ex-substitute' with special replacements."
  :tags '(vimp ex search)
  (ert-info ("Substitute upper first on first match in line")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1" [return])
      "[x]xx Foo bar foo bar foo bar"))
  (ert-info ("Substitute upper first on first match in line with confirm")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1/c" [return] "y")
      "[x]xx Foo bar foo bar foo bar"))
  (ert-info ("Substitute upper first on whole line")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1/g" [return])
      "[x]xx Foo Bar Foo Bar Foo Bar"))
  (ert-info ("Substitute upper first on whole line")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/\\(foo\\|bar\\)/\\u\\1/gc" [return] "yynyyn")
      "[x]xx Foo Bar foo Bar Foo bar"))
  (ert-info ("Substitute upper/lower on first match in line")
    (vimp-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1" [return])
      "[x]xx bar_FOO foo BAR foo BAR"))
  (ert-info ("Substitute upper/lower on first match in line with confirm")
    (vimp-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1/c" [return] "y")
      "[x]xx bar_FOO foo BAR foo BAR"))
  (ert-info ("Substitute upper/lower on whole line")
    (vimp-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1/g" [return])
      "[x]xx bar_FOO bar_FOO bar_FOO"))
  (ert-info ("Substitute upper/lower on whole line")
    (vimp-test-buffer
      "[x]xx foo BAR foo BAR foo BAR"
      (":s/\\(f[[:alpha:]]*\\>\\)\\s-*\\(b[[:alpha:]]*\\>\\)/\\L\\2_\\e\\U\\1/gc" [return] "yny")
      "[x]xx bar_FOO foo BAR bar_FOO"))
  (ert-info ("Substitute with escaped characters in replacement")
    (vimp-test-buffer
      "[a]bcXdefXghiXjkl\n"
      (":s/X/\\|\\/\\|/g" [return])
      "[a]bc|/|def|/|ghi|/|jkl\n"))
  (ert-info ("Substitute with register")
    (vimp-test-buffer
      "[a]bc\niiiXiiiXiiiXiii\n"
      ("\"ayiwj:s/X/\\=@a/g" [return])
      "abc\n[i]iiabciiiabciiiabciii\n")))

(ert-deftest vimp-test-ex-repeat-substitute-replacement ()
  "Test `vimp-ex-substitute' with repeating of previous substitutions."
  :tags '(vimp ex search)
  (ert-info ("Repeat previous pattern")
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar"
      (":s//BBB" [return])
      "[x]xx AAA bar BBB bar foo bar"
      ("/bar" [return] ":s//CCC" [return])
      "[x]xx AAA CCC BBB bar foo bar"
      (":s/ar/XX" [return])
      "[x]xx AAA CCC BBB bXX foo bar"
      (":s//YY" [return])
      "[x]xx AAA CCC BBB bXX foo bYY"))
  (ert-info ("Repeat previous replacement")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar"
      (":s/bar/~" [return])
      "[x]xx AAA AAA foo bar foo bar"))
  (ert-info ("Repeat with previous flags")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar"
      (":s/bar/BBB/&" [return])
      "[x]xx AAA BBB AAA BBB AAA BBB"))
  (ert-info ("Repeat previous substitute without flags")
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:s" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar foo bar foo bar"
      ("/bar" [return] ":s" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar foo bar")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j&")
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar foo bar foo bar")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:&" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar foo bar foo bar"))
  (ert-info ("Repeat previous substitute with the same flags")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:s//~/&" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar AAA bar")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("j:&&" [return])
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar AAA bar"))
  (ert-info ("Repeat previous substitute with new flags")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("j:s g" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx AAA bar AAA bar AAA bar")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("j:& g" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx AAA bar AAA bar AAA bar"))
  (ert-info ("Repeat with previous search pattern")
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("/bar" [return])
      "xxx AAA [b]ar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":2s rg" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx foo AAA foo AAA foo AAA")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA" [return])
      "[x]xx AAA bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      ("/bar" [return])
      "xxx AAA [b]ar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":2~ g" [return])
      "xxx AAA bar foo bar foo bar\n[x]xx foo AAA foo AAA foo AAA"))
  (ert-info ("Repeat previous substitute globally")
    (vimp-test-buffer
      "[x]xx foo bar foo bar foo bar\nxxx foo bar foo bar foo bar"
      (":s/foo/AAA/g" [return])
      "[x]xx AAA bar AAA bar AAA bar\nxxx foo bar foo bar foo bar"
      ("g&")
      "xxx AAA bar AAA bar AAA bar\n[x]xx AAA bar AAA bar AAA bar")))

(ert-deftest vimp-test-ex-regex-without-case ()
  "Test `vimp-ex-regex-without-case'"
  :tags '(vimp ex search)
  (should (equal (vimp-ex-regex-without-case "cdeCDE")
                 "cdeCDE"))
  (should (equal (vimp-ex-regex-without-case "\\ccde\\CCDE")
                 "cdeCDE"))
  (should (equal (vimp-ex-regex-without-case "\\\\ccde\\\\CCDE")
                 "\\\\ccde\\\\CCDE"))
  (should (equal (vimp-ex-regex-without-case "\\\\\\ccde\\\\\\CCDE")
                 "\\\\cde\\\\CDE")))

(ert-deftest vimp-test-ex-regex-case ()
  "Test `vimp-ex-regex-case'"
  :tags '(vimp ex search)
  (should (equal (vimp-ex-regex-case "cde" 'smart) 'insensitive))
  (should (equal (vimp-ex-regex-case "cDe" 'smart) 'sensitive))
  (should (equal (vimp-ex-regex-case "cde" 'sensitive) 'sensitive))
  (should (equal (vimp-ex-regex-case "cde" 'insensitive) 'insensitive))
  (should (equal (vimp-ex-regex-case "\\ccde" 'smart) 'insensitive))
  (should (equal (vimp-ex-regex-case "\\cCde" 'smart) 'insensitive))
  (should (equal (vimp-ex-regex-case "\\Ccde" 'smart) 'sensitive))
  (should (equal (vimp-ex-regex-case "\\CCde" 'smart) 'sensitive))
  (should (equal (vimp-ex-regex-case "\\ccd\\Ce" 'smart) 'insensitive))
  (should (equal (vimp-ex-regex-case "\\cCd\\Ce" 'smart) 'insensitive))
  (should (equal (vimp-ex-regex-case "\\Ccd\\ce" 'smart) 'sensitive))
  (should (equal (vimp-ex-regex-case "\\CCd\\ce" 'smart) 'sensitive)))

(ert-deftest vimp-test-ex-search ()
  "Test vimp internal search."
  :tags '(vimp ex search)
  (vimp-without-display
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (ert-info ("Test smart case insensitive")
      (vimp-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/you" [return])
        "start [y]ou YOU You you YOU You"
        ("n")
        "start you [Y]OU You you YOU You"
        ("n")
        "start you YOU [Y]ou you YOU You"
        ("n")
        "start you YOU You [y]ou YOU You"))
    (ert-info ("Test smart case sensitive")
      (vimp-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/You" [return])
        "start you YOU [Y]ou you YOU You"
        ("n")
        "start you YOU You you YOU [Y]ou"))
    (ert-info ("Test insensitive")
      (vimp-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/\\cyou" [return])
        "start [y]ou YOU You you YOU You"
        ("n")
        "start you [Y]OU You you YOU You"
        ("n")
        "start you YOU [Y]ou you YOU You"
        ("n")
        "start you YOU You [y]ou YOU You"))
    (ert-info ("Test sensitive")
      (vimp-test-buffer
        "[s]tart you YOU You you YOU You"
        ("/\\Cyou" [return])
        "start [y]ou YOU You you YOU You"
        ("n")
        "start you YOU You [y]ou YOU You"))
    (ert-info ("Test failing search does not move point")
      (vimp-test-buffer
        "foo [f]oo foo\nbar bar2 bar\nbaz baz baz\n"
        (error search-failed "/foofoo" [return])
        "foo [f]oo foo\nbar bar2 bar\nbaz baz baz\n"
        ("/bar2" [return])
        "foo foo foo\nbar [b]ar2 bar\nbaz baz baz\n"
        ("dw")
        "foo foo foo\nbar [b]ar\nbaz baz baz\n"
        (error search-failed "n")
        "foo foo foo\nbar [b]ar\nbaz baz baz\n"
        (error search-failed "N")
        "foo foo foo\nbar [b]ar\nbaz baz baz\n"))))

(ert-deftest vimp-test-ex-search-offset ()
  "Test search offsets."
  :tags '(vimp ex search)
  (vimp-without-display
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (ert-info ("Test line offsets")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/2")
        "foo foo\nbar bar\nbaz baz\n[A]nother line\nAnd yet another line"
        ("?bar?-")
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/r bar/")
        "foo foo\nba[r] bar\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test end offsets")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e")
        "foo foo\nba[r] bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/baz/e+2")
        "foo foo\nbar bar\nbaz [b]az\nAnother line\nAnd yet another line"
        ("/line/e-1")
        "foo foo\nbar bar\nbaz baz\nAnother li[n]e\nAnd yet another line"))
    (ert-info ("Test begin offsets")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/b")
        "foo foo\n[b]ar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/baz/b+2")
        "foo foo\nbar bar\nba[z] baz\nAnother line\nAnd yet another line"
        ("/line/b-")
        "foo foo\nbar bar\nbaz baz\nAnother[ ]line\nAnd yet another line"))
    (ert-info ("Test search-next with offset")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/ ba/+1" [return])
        "foo foo\nbar bar\n[b]az baz\nAnother line\nAnd yet another line"
        ("n")
        "foo foo\nbar bar\nbaz baz\n[A]nother line\nAnd yet another line"))
    (ert-info ("Test search next after /$")
      (vimp-test-buffer
        "[l]ine 1\nline 2\n\n\line 4\n"
        ("/$" [return])
        "line [1]\nline 2\n\n\line 4\n"
        ("n")
        "line 1\nline [2]\n\n\line 4\n"
        ("n")
        "line 1\nline 2\n[\n]\line 4\n"
        ("n")
        "line 1\nline 2\n\n\line [4]\n"))))

(ert-deftest vimp-test-ex-search-pattern-offset ()
  "Test pattern offsets."
  :tags '(vimp ex search)
  (vimp-without-display
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (ert-info ("Test simple pattern offsets")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/;/foo" [return])
        "foo foo\nbar bar\n[f]oo foo\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test simple pattern offsets in backward direction")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/;?foo" [return])
        "foo [f]oo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Ensure second pattern is used for search repeat")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nfoo foo\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/;?foo" [return] "n")
        "foo foo\nbar bar\n[f]oo foo\nbaz baz\nAnother line\nAnd yet another line"))))

(ert-deftest vimp-test-ex-search-repeat ()
  "Test repeat of search."
  :tags '(vimp ex search)
  (vimp-without-display
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (ert-info ("Test repeat of simple pattern")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar" [return] "/" [return])
        "foo foo\nbar [b]ar\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of simple pattern with new offset")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar" [return] "//e" [return])
        "foo foo\nbar ba[r]\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of pattern with offset")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e" [return] "/" [return])
        "foo foo\nbar ba[r]\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of pattern with offset without offset")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e" [return] "//" [return])
        "foo foo\nbar [b]ar\nbaz baz\nAnother line\nAnd yet another line"))
    (ert-info ("Test repeat of pattern with offset with new offset")
      (vimp-test-buffer
        "[f]oo foo\nbar bar\nbaz baz\nAnother line\nAnd yet another line"
        ("/bar/e" [return] "//b+1" [return])
        "foo foo\nbar b[a]r\nbaz baz\nAnother line\nAnd yet another line"))))

(ert-deftest vimp-test-ex-search-word ()
  "Test search for word under point."
  :tags '(vimp ex search)
  (vimp-without-display
    (vimp-select-search-module 'vimp-search-module 'vimp-search)
    (setq vimp-ex-search-history nil)
    (vimp-test-buffer
      "so[m]e text with a strange word
and here some other stuff
maybe we need one line more with some text\n"
      (setq vimp-symbol-word-search nil)
      ("*")
      "some text with a strange word
and here [s]ome other stuff
maybe we need one line more with some text\n"
      ("n")
      "some text with a strange word
and here some other stuff
maybe we need one line more with [s]ome text\n"
      (ert-info ("Search history")
        (should (equal vimp-ex-search-history '("\\<some\\>"))))
      ("*")
      "[s]ome text with a strange word
and here some other stuff
maybe we need one line more with some text\n"
      (ert-info ("Search history with double pattern")
        (should (equal vimp-ex-search-history '("\\<some\\>")))))
    (ert-info ("Test unbounded search")
      (vimp-select-search-module 'vimp-search-module 'vimp-search)
      (setq vimp-ex-search-history nil)
      (vimp-test-buffer
        "[s]ymbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        ("*")
        (setq vimp-symbol-word-search nil)
        "symbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother [s]ymbol\n"
        ("ggg*")
        "symbol\n(defun my-[s]ymbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        (should (equal vimp-ex-search-history '("symbol" "\\<symbol\\>")))
        ("n")
        "symbol\n(defun my-symbolfunc ())\n(defvar my-[s]ymbolvar)\nanother symbol\n"))
    (ert-info ("Test symbol search")
      (vimp-select-search-module 'vimp-search-module 'vimp-search)
      (vimp-test-buffer
        "(defun my-s[y]mbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        (setq vimp-symbol-word-search t)
        ("*")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n([m]y-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        ("n")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 ([m]y-symbol-func))\n"))))

(ert-deftest vimp-test-isearch-word ()
  "Test isearch for word under point."
  :tags '(vimp isearch)
  (vimp-without-display
    (vimp-select-search-module 'vimp-search-module 'isearch)
    (vimp-test-buffer
      "so[m]e text with a strange word
and here some other stuff
maybe we need one line more with some text\n"
      (setq vimp-symbol-word-search nil)
      ("*")
      "some text with a strange word
and here [s]ome other stuff
maybe we need one line more with some text\n"
      ("n")
      "some text with a strange word
and here some other stuff
maybe we need one line more with [s]ome text\n"
      ("*")
      "[s]ome text with a strange word
and here some other stuff
maybe we need one line more with some text\n")
    (ert-info ("Test unbounded search")
      (vimp-select-search-module 'vimp-search-module 'isearch)
      (vimp-test-buffer
        "[s]ymbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        (setq vimp-symbol-word-search nil)
        ("*")
        "symbol\n(defun my-symbolfunc ())\n(defvar my-symbolvar)\nanother [s]ymbol\n"
        ("ggg*")
        "symbol\n(defun my-[s]ymbolfunc ())\n(defvar my-symbolvar)\nanother symbol\n"
        ("n")
        "symbol\n(defun my-symbolfunc ())\n(defvar my-[s]ymbolvar)\nanother symbol\n"))
    (ert-info ("Test symbol search")
      (vimp-select-search-module 'vimp-search-module 'isearch)
      (vimp-test-buffer
        "(defun my-s[y]mbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        (setq vimp-symbol-word-search t)
        ("*")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n([m]y-symbol-func)\n(setq my-symbol-func2 (my-symbol-func))\n"
        ("n")
        "(defun my-symbol-func ())\n(defvar my-symbol-var)\n(my-symbol-func)\n(setq my-symbol-func2 ([m]y-symbol-func))\n"))))

(ert-deftest vimp-test-read ()
  "Test of `vimp-read'"
  :tags '(vimp ex)
  (vimp-without-display
    (ert-info ("Test insertion of file with trailing newline")
      (vimp-with-temp-file name
          "temp file 1\ntemp file 2\n"
        (ert-info ("At first line")
          (vimp-test-buffer
            "[l]ine 1\nline 2"
            ((vconcat ":read " name [return]))
            "line 1\n[t]emp file 1\ntemp file 2\nline 2"))
        (ert-info ("At last line")
          (vimp-test-buffer
            "line 1\n[l]ine 2"
            ((vconcat ":read " name [return]))
            "line 1\nline 2\n[t]emp file 1\ntemp file 2\n"))
        (ert-info ("After specified line number")
          (vimp-test-buffer
            "[l]ine 1\nline 2\nline 3\nline 4\line 5"
            ((vconcat ":3read " name [return]))
            "line 1\nline 2\nline 3\n[t]emp file 1\ntemp file 2\nline 4\line 5"))
        (ert-info ("After specified line 0")
          (vimp-test-buffer
            "line 1\nline [2]\nline 3\nline 4\line 5"
            ((vconcat ":0read " name [return]))
            "[t]emp file 1\ntemp file 2\nline 1\nline 2\nline 3\nline 4\line 5"))))
    (ert-info ("Test insertion of file without trailing newline")
      (vimp-with-temp-file name
          "temp file 1\ntemp file 2"
        (vimp-test-buffer
          "[l]ine 1\nline 2"
          ((vconcat ":read " name [return]))
          "line 1\n[t]emp file 1\ntemp file 2\nline 2")))
    (ert-info ("Test insertion of shell command")
      (ert-info ("with space")
        (vimp-test-buffer
          "[l]line 1\nline 2"
          (":read !echo cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2"))
      (ert-info ("without space")
        (vimp-test-buffer
          "[l]line 1\nline 2"
          (":read!echo cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2")))
    (ert-info ("Test insertion of shell command without trailing newline")
      (ert-info ("with space")
        (vimp-test-buffer
          "[l]line 1\nline 2"
          (":read !echo -n cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2"))
      (ert-info ("without space")
        (vimp-test-buffer
          "[l]line 1\nline 2"
          (":read!echo -n cmd line 1" [return])
          "line 1\n[c]md line 1\nline 2")))))

(ert-deftest vimp-test-shell-command ()
  "Test `vimp-shell-command'."
  (ert-info ("ex shell command")
    (vimp-test-buffer
      "[l]ine 5\nline 4\nline 3\nline 2\nline 1\n"
      (":2,3!sort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with count")
    (vimp-test-buffer
      "line 5\n[l]ine 4\nline 3\nline 2\nline 1\n"
      ("2!!sort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with motion")
    (vimp-test-buffer
      "line 5\n[l]ine 4\nline 3\nline 2\nline 1\n"
      ("!jsort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with backward motion")
    (vimp-test-buffer
      "line 5\nline 4\n[l]ine 3\nline 2\nline 1\n"
      ("!ksort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n"))
  (ert-info ("shell command operator with visual selection")
    (vimp-test-buffer
      "line 5\n[l]ine 4\nline 3\nline 2\nline 1\n"
      ("vj!sort" [return])
      "line 5\n[l]ine 3\nline 4\nline 2\nline 1\n")))

(ert-deftest vimp-test-global ()
  "Test `vimp-ex-global'."
  :tags '(vimp ex)
  (ert-info ("global delete")
    (vimp-test-buffer
      "[n]o 1\nno 2\nno 3\nyes 4\nno 5\nno 6\nno 7\n"
      (":g/yes/d" [return])
      "no 1\nno 2\nno 3\n[n]o 5\nno 6\nno 7\n"))
  (ert-info ("global substitute")
    (vimp-test-buffer
      "[n]o 1\nno 2\nno 3\nyes 4\nno 5\nno 6\nno 7\n"
      (":g/no/s/[3-6]/x" [return])
      "no 1\nno 2\nno x\nyes 4\nno x\nno x\n[n]o 7\n"
      ("u")
      "no 1\nno 2\nno [3]\nyes 4\nno 5\nno 6\nno 7\n")))

(ert-deftest vimp-test-normal ()
  "Test `vimp-ex-normal'."
  :tags '(vimp ex)
  (let (vimp-want-fine-undo)
    (vimp-test-buffer
      "[l]ine 1\nline 2\nline 3\nline 4\nline 5\n"
      (":normal lxIABC" [escape] "AXYZ" [return])
      "ABClne 1XY[Z]\nline 2\nline 3\nline 4\nline 5\n"
      (":3,4normal lxIABC" [escape] "AXYZ" [return])
      "ABClne 1XYZ\nline 2\nABClne 3XYZ\nABClne 4XY[Z]\nline 5\n"
      ("u")
      "ABClne 1XYZ\nline 2\nl[i]ne 3\nline 4\nline 5\n")))

(ert-deftest vimp-test-copy ()
  :tags '(vimp ex)
  "Test `vimp-copy'."
  (ert-info ("Copy to last line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3copy$")
      "line1\nline2\nline3\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Copy to last incomplete line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":2,3copy$")
      "line1\nline2\nline3\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Copy incomplete line to last incomplete line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":4,5copy$")
      "line1\nline2\nline3\nline4\nline5\nline4\n[l]ine5\n"))
  (ert-info ("Copy to first line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3copy0")
      "line2\n[l]ine3\nline1\nline2\nline3\nline4\nline5\n"))
  (ert-info ("Copy to intermediate line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,4copy2")
      "line1\nline2\nline2\nline3\n[l]ine4\nline3\nline4\nline5\n"))
  (ert-info ("Copy to current line")
    (vimp-test-buffer
      "line1\nline2\nline3\nli[n]e4\nline5\n"
      (":2,4copy.")
      "line1\nline2\nline3\nline4\nline2\nline3\n[l]ine4\nline5\n")))

(ert-deftest vimp-test-move ()
  :tags '(vimp ex)
  "Test `vimp-move'."
  (ert-info ("Move to last line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3move$")
      "line1\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Move to last incomplete line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":2,3move$")
      "line1\nline4\nline5\nline2\n[l]ine3\n"))
  (ert-info ("Move incomplete line to last incomplete line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5"
      (":4,5move$")
      "line1\nline2\nline3\nline4\n[l]ine5\n"))
  (ert-info ("Move to first line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3move0")
      "line2\n[l]ine3\nline1\nline4\nline5\n"))
  (ert-info ("Move to intermediate line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,4move2")
      "line1\nline2\nline3\n[l]ine4\nline5\n"))
  (ert-info ("Move to other line")
    (vimp-test-buffer
      "[l]ine1\nline2\nline3\nline4\nline5\n"
      (":2,3move4")
      "line1\nline4\nline2\n[l]ine3\nline5\n"))
  (ert-info ("Move to current line")
    (vimp-test-buffer
      "line1\nline2\nline3\nli[n]e4\nline5\n"
      (":2,4move.")
      "line1\nline2\nline3\n[l]ine4\nline5\n")))

(ert-deftest vimp-test-write ()
  :tags '(vimp ex)
  "Test `vimp-write'."
  (ert-info ("Write open file")
    (vimp-with-temp-file filename "line1\nline2\nline3\n"
      (vimp-test-buffer
        ((vconcat ":e " filename [return]))
        "[l]ine1\nline2\nline3\n"
        ("Galine4\nline5\n" [escape])
        "line1\nline2\nline3\nline4\nline5\n"
        (":w")
        (file filename "line1\nline2\nline3\nline4\nline5\n"))))
  (let ((filename (vimp-temp-filename)))
    (ert-info ("Write current buffer to new file")
      (vimp-test-buffer
        "[l]ine1\nline2\nline3\nline4\nline5\n"
        ((vconcat ":w " filename [return]))
        (file filename "line1\nline2\nline3\nline4\nline5\n")
        (delete-file filename)))
    (ert-info ("Write part of a buffer")
      (vimp-test-buffer
        "[l]ine1\nline2\nline3\nline4\nline5\n"
        ((vconcat ":2,3w " filename [return]))
        (file filename "line2\nline3\n")
        (delete-file filename)))
    (ert-info ("Appending a file")
      (vimp-test-buffer
        "[l]ine1\nline2\nline3\nline4\nline5\n"
        ((vconcat ":4w " filename [return]))
        (file filename "line4\n")
        ((vconcat ":1,2w >>" filename [return]))
        (file filename "line4\nline1\nline2\n")
        ((vconcat ":w >> " filename [return]))
        (file filename
              "line4\nline1\nline2\nline1\nline2\nline3\nline4\nline5\n")
        (delete-file filename)))))

(ert-deftest vimp-test-ex-sort ()
  :tags '(vimp ex)
  "Text ex command :sort `vimp-ex-sort`."
  (ert-info ("Plain sort")
    (vimp-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort")
      "[T]EST\ntEst\ntesT\ntest\ntest\nzzyy\n"))
  (ert-info ("Reverse sort")
    (vimp-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort!")
      "[z]zyy\ntest\ntest\ntesT\ntEst\nTEST\n"))
  (ert-info ("case insensitive")
    (vimp-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort i")
      "[t]est\ntEst\ntesT\nTEST\ntest\nzzyy\n"))
  (ert-info ("unique")
    (vimp-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort u")
      "[T]EST\ntEst\ntesT\ntest\nzzyy\n"))
  (ert-info ("case insensitive and unique")
    (vimp-test-buffer
      "[z]zyy\ntest\ntEst\ntesT\nTEST\ntest\n"
      (":sort iu")
      "[t]est\nzzyy\n")))

;;; Command line window

(ert-deftest vimp-test-command-window-ex ()
  "Test command line window for ex commands"
  (vimp-test-buffer
    "[f]oo foo foo"
    (":s/foo/bar" [return])
    "[b]ar foo foo"
    (":s/foo/baz" [return])
    "[b]ar baz foo"
    ("q:")
    "s/foo/bar\ns/foo/baz\n[ ]"
    ("kk:s/bar/quz" [return])
    "[s]/foo/quz\ns/foo/baz\n "
    ("fzrx")
    "s/foo/qu[x]\ns/foo/baz\n "
    ([return])
    "[b]ar baz qux"
    (should (equal (car vimp-ex-history)
                   "s/foo/qux"))))

(ert-deftest vimp-test-command-window-recursive ()
  "Test that recursive command windows shouldn't be allowed"
  (let ((vimp-command-window-height 0))
    (vimp-test-buffer
      "[f]oo foo foo"
      (":s/foo/bar" [return])
      ("q:")
      (should-error (execute-kbd-macro "q:")))))

(ert-deftest vimp-test-command-window-noop ()
  "Test that executing a blank command does nothing"
  (vimp-test-buffer
    "[f]oo foo foo"
    ("q:")
    "[ ]"
    ([return])
    "[f]oo foo foo"))

(ert-deftest vimp-test-command-window-multiple ()
  "Test that multiple command line windows can't be visible at the same time"
  (let ((vimp-command-window-height 0))
    (vimp-test-buffer
      "[f]oo foo foo"
      ("q:")
      (let ((num-windows (length (window-list))))
        (select-window (previous-window))
        (execute-kbd-macro "q:")
        (should (= (length (window-list)) num-windows))))))

(defmacro vimp-with-both-search-modules (&rest body)
  `(mapc (lambda (search-module)
           (setq vimp-search-forward-history nil
                 vimp-search-backward-history nil)
           (vimp-select-search-module 'vimp-search-module search-module)
           ,@body)
         '(isearch vimp-search)))

(ert-deftest vimp-test-command-window-search-history ()
  "Test command window with forward and backward search history"
  (vimp-with-both-search-modules
   (vimp-test-buffer
     "[f]oo bar baz qux one two three four"
     ("/qux" [return])
     "foo bar baz [q]ux one two three four"
     ("/three" [return])
     "foo bar baz qux one two [t]hree four"
     ("?bar" [return])
     "foo [b]ar baz qux one two three four"
     ("/four" [return])
     "foo bar baz qux one two three [f]our"
     ("?baz" [return])
     "foo bar [b]az qux one two three four"
     ("q/")
     "qux\nthree\nfour\n[ ]"
     ("k" [return])
     "foo bar baz qux one two three [f]our"
     ("0N")
     "foo bar baz qux one two three [f]our"
     ("q?")
     "bar\nbaz\n[ ]"
     ("k$rr" [return])
     "foo [b]ar baz qux one two three four"
     (should-error
      (progn (execute-kbd-macro "q/iNOT THERE")
             (execute-kbd-macro [return])))
     "foo [b]ar baz qux one two three four")))

(ert-deftest vimp-test-command-window-search-word ()
  "Test command window history when searching for word under cursor"
  (vimp-with-both-search-modules
   (vimp-test-buffer
     "[f]oo bar foo bar foo"
     ("**")
     "foo bar foo bar [f]oo"
     ("B#")
     "foo [b]ar foo bar foo"
     ("q/k" [return])
     "foo bar [f]oo bar foo"
     ("q?k" [return])
     "foo [b]ar foo bar foo")))

;;; Utilities

(ert-deftest vimp-test-parser ()
  "Test `vimp-parser'"
  (let ((grammar '((number "[0-9]+" #'string-to-number)
                   (plus "\\+" #'intern)
                   (minus "-" #'intern)
                   (operator
                    plus
                    minus)
                   (sign
                    ((\? operator) #'$1))
                   (signed-number
                    (sign number))
                   (inc
                    (number #'(lambda (n) (1+ n))))
                   (expr
                    (number operator number)
                    ("2" #'"1+1"))
                   (epsilon nil))))
    (ert-info ("Nothing")
      (should (equal (vimp-parser "1+2" nil grammar t)
                     nil))
      (should (equal (vimp-parser "1+2" nil grammar)
                     '(nil . "1+2")))
      (should (equal (vimp-parser "1+2" 'epsilon grammar t)
                     nil))
      (should (equal (vimp-parser "1+2" 'epsilon grammar)
                     '(nil . "1+2"))))
    (ert-info ("Strings")
      (should (equal (vimp-parser "1" 'number grammar t)
                     '((string-to-number "1"))))
      (should (equal (vimp-parser "11" 'number grammar)
                     '((string-to-number "11") . ""))))
    (ert-info ("Sequences")
      (should (equal (vimp-parser "1" '(number) grammar t)
                     '((list (string-to-number "1")))))
      (should (equal (vimp-parser "1+2" '(number operator number) grammar t)
                     '((list
                        (string-to-number "1")
                        (intern "+")
                        (string-to-number "2"))))))
    (ert-info ("Symbols")
      (should (equal (vimp-parser "+" 'plus grammar t)
                     '((intern "+"))))
      (should (equal (vimp-parser "+" 'operator grammar t)
                     '((intern "+"))))
      (should (equal (vimp-parser "1" 'number grammar t)
                     '((string-to-number "1")))))
    (ert-info ("Whitespace")
      (should (equal (vimp-parser " 1" 'number grammar t)
                     '((string-to-number "1")))))
    (ert-info ("One or more")
      (should (equal (vimp-parser "1 2 3" '(+ number) grammar t)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2")
                        (string-to-number "3")))))
      (should (equal (vimp-parser "1 2 3" '(* number) grammar t)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2")
                        (string-to-number "3")))))
      (should (equal (vimp-parser "1 2 3" '(\? number) grammar)
                     '((string-to-number "1") . " 2 3")))
      (should (equal (vimp-parser "1 2 3" '(\? number number) grammar)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2"))
                       . " 3")))
      (should (equal (vimp-parser "1 2 3" '(number (\? number)) grammar)
                     '((list
                        (string-to-number "1")
                        (string-to-number "2"))
                       . " 3")))
      (should (equal (vimp-parser "1 2 3" '(number (\? number number)) grammar)
                     '((list
                        (string-to-number "1")
                        (list
                         (string-to-number "2")
                         (string-to-number "3")))
                       . "")))
      (should (equal (vimp-parser "1 a 3" '(number (\? number)) grammar)
                     '((list
                        (string-to-number "1")
                        nil)
                       . " a 3")))
      (should (equal (vimp-parser "1" 'signed-number grammar t t)
                     '((signed-number (sign "") (number "1")) . ""))))
    (ert-info ("Lookahead")
      (should (equal (vimp-parser "foobar" '("foo" (& "bar")) grammar)
                     '((list "foo") . "bar")))
      (should (equal (vimp-parser "foobar" '("foo" (! "bar")) grammar)
                     nil))
      (should (equal (vimp-parser "foobar" '("foo" (& "baz")) grammar)
                     nil))
      (should (equal (vimp-parser "foobar" '("foo" (! "baz")) grammar)
                     '((list "foo") . "bar"))))
    (ert-info ("Semantic actions")
      (should (equal (vimp-parser "1" 'inc grammar t)
                     '((funcall (lambda (n)
                                  (1+ n))
                                (string-to-number "1")))))
      (should (equal (vimp-parser "1+1" 'expr grammar t)
                     '((list
                        (string-to-number "1")
                        (intern "+")
                        (string-to-number "1")))))
      (should (equal (vimp-parser "2" 'expr grammar t)
                     '((list (string-to-number "1")
                             (intern "+")
                             (string-to-number "1"))))))))

(ert-deftest vimp-test-delimited-arguments ()
  "Test `vimp-delimited-arguments'"
  :tags '(vimp util)
  (ert-info ("Any number of arguments")
    (should (equal (vimp-delimited-arguments "/a/b/c/")
                   '("a" "b" "c")))
    (should (equal (vimp-delimited-arguments "/a/b/c")
                   '("a" "b" "c")))
    (should (equal (vimp-delimited-arguments "/a/b//")
                   '("a" "b" "")))
    (should (equal (vimp-delimited-arguments "/a///")
                   '("a" "" "")))
    (should (equal (vimp-delimited-arguments "/a/   ")
                   '("a" "   ")))
    (should (equal (vimp-delimited-arguments "/a/")
                   '("a")))
    (should (equal (vimp-delimited-arguments "//b//")
                   '("" "b" "")))
    (should (equal (vimp-delimited-arguments "/a//c")
                   '("a" "" "c")))
    (should (equal (vimp-delimited-arguments "////")
                   '("" "" "")))
    (should (equal (vimp-delimited-arguments "/")
                   nil))
    (should (equal (vimp-delimited-arguments "    ")
                   nil))
    (should (equal (vimp-delimited-arguments "")
                   nil)))
  (ert-info ("Two arguments")
    (should (equal (vimp-delimited-arguments "/a/b/c" 2)
                   '("a" "b/c")))
    (should (equal (vimp-delimited-arguments "/a/b/" 2)
                   '("a" "b")))
    (should (equal (vimp-delimited-arguments "/a/b" 2)
                   '("a" "b")))
    (should (equal (vimp-delimited-arguments "/a//" 2)
                   '("a" "")))
    (should (equal (vimp-delimited-arguments "/a/   " 2)
                   '("a" "   ")))
    (should (equal (vimp-delimited-arguments "/a/" 2)
                   '("a" nil)))
    (should (equal (vimp-delimited-arguments "/a" 2)
                   '("a" nil)))
    (should (equal (vimp-delimited-arguments "    " 2)
                   '(nil nil)))
    (should (equal (vimp-delimited-arguments "" 2)
                   '(nil nil))))
  (ert-info ("One argument")
    (should (equal (vimp-delimited-arguments "/a/b/c" 1)
                   '("a/b/c")))
    (should (equal (vimp-delimited-arguments "/a/   " 1)
                   '("a")))
    (should (equal (vimp-delimited-arguments "/a/" 1)
                   '("a")))
    (should (equal (vimp-delimited-arguments "/a" 1)
                   '("a")))
    (should (equal (vimp-delimited-arguments "/" 1)
                   '(nil)))
    (should (equal (vimp-delimited-arguments "    " 1)
                   '(nil)))
    (should (equal (vimp-delimited-arguments "" 1)
                   '(nil))))
  (ert-info ("Zero arguments")
    (should (equal (vimp-delimited-arguments "/a" 0)
                   nil))
    (should (equal (vimp-delimited-arguments "/" 0)
                   nil))
    (should (equal (vimp-delimited-arguments "    " 0)
                   nil))
    (should (equal (vimp-delimited-arguments "" 0)
                   nil))))

(ert-deftest vimp-test-concat-charsets ()
  "Test `vimp-concat-charsets'"
  :tags '(vimp util)
  (ert-info ("Bracket")
    (should (equal (vimp-concat-charsets "abc" "]def")
                   "]abcdef")))
  (ert-info ("Complement")
    (should (equal (vimp-concat-charsets "^abc" "def")
                   "^abcdef"))
    (should (equal (vimp-concat-charsets "^abc" "^def")
                   "^abcdef")))
  (ert-info ("Hyphen")
    (should (equal (vimp-concat-charsets "abc" "-def")
                   "-abcdef"))
    (should (equal (vimp-concat-charsets "^abc" "-def")
                   "^-abcdef")))
  (ert-info ("Newline")
    (should (equal (vimp-concat-charsets "^ \t\r\n" "[:word:]_")
                   "^ \t\r\n[:word:]_"))))

(ert-deftest vimp-test-properties ()
  "Test `vimp-get-property' and `vimp-put-property'"
  :tags '(vimp util)
  (let (alist)
    (ert-info ("Set properties")
      (vimp-put-property 'alist 'wibble :foo t)
      (should (equal alist '((wibble . (:foo t)))))
      (vimp-put-property 'alist 'wibble :bar nil)
      (should (equal alist '((wibble . (:foo t :bar nil)))))
      (vimp-put-property 'alist 'wobble :foo nil :bar nil :baz t)
      (should (equal alist '((wobble . (:foo nil :bar nil :baz t))
                             (wibble . (:foo t :bar nil))))))
    (ert-info ("Get properties")
      (should (vimp-get-property alist 'wibble :foo))
      (should-not (vimp-get-property alist 'wibble :bar))
      (should-not (vimp-get-property alist 'wobble :foo))
      (should-not (vimp-get-property alist 'wibble :baz))
      (should (equal (vimp-get-property alist t :foo)
                     '((wibble . t) (wobble . nil))))
      (should (equal (vimp-get-property alist t :bar)
                     '((wibble . nil) (wobble . nil))))
      (should (equal (vimp-get-property alist t :baz)
                     '((wobble . t)))))))

(ert-deftest vimp-test-filter-list ()
  "Test `vimp-filter-list'"
  :tags '(vimp util)
  (ert-info ("Return filtered list")
    (should (equal (vimp-filter-list #'null '(nil)) nil))
    (should (equal (vimp-filter-list #'null '(nil 1)) '(1)))
    (should (equal (vimp-filter-list #'null '(nil 1 2 nil)) '(1 2)))
    (should (equal (vimp-filter-list #'null '(nil nil 1)) '(1)))
    (should (equal (vimp-filter-list #'null '(nil 1 nil 2 nil 3))
                   '(1 2 3))))
  (ert-info ("Remove matches by side-effect when possible")
    (let (list)
      (setq list '(1 nil))
      (vimp-filter-list #'null list)
      (should (equal list '(1)))

      (setq list '(1 nil nil))
      (vimp-filter-list #'null list)
      (should (equal list '(1)))

      (setq list '(1 nil nil 2))
      (vimp-filter-list #'null list)
      (should (equal list '(1 2)))

      (setq list '(1 nil 2 nil 3))
      (vimp-filter-list #'null list)
      (should (equal list '(1 2 3))))))

(ert-deftest vimp-test-concat-lists ()
  "Test `vimp-concat-lists' and `vimp-concat-alists'"
  :tags '(vimp util)
  (ert-info ("Remove duplicates across lists")
    (should (equal (vimp-concat-lists
                    nil '(a b) '(b c))
                   '(a b c))))
  (ert-info ("Remove duplicates inside lists")
    (should (equal (vimp-concat-lists
                    '(a a b) nil '(b c) nil)
                   '(a b c))))
  (ert-info ("Remove duplicate associations")
    (should (equal (vimp-concat-alists
                    '((a . b)) '((a . c)))
                   '((a . c))))
    (should-not (equal (vimp-concat-lists
                        '((a . b)) '((a . c)))
                       '((a . b))))))

(ert-deftest vimp-test-sort ()
  "Test `vimp-sort' and `vimp-swap'"
  :tags '(vimp util)
  (let (a b c d)
    (ert-info ("Two elements")
      (setq a 2 b 1)
      (vimp-sort a b)
      (should (= a 1))
      (should (= b 2))
      (vimp-swap a b)
      (should (= a 2))
      (should (= b 1)))
    (ert-info ("Three elements")
      (setq a 3 b 1 c 2)
      (vimp-sort a b c)
      (should (= a 1))
      (should (= b 2))
      (should (= c 3)))
    (ert-info ("Four elements")
      (setq a 4 b 3 c 2 d 1)
      (vimp-sort a b c d)
      (should (= a 1))
      (should (= b 2))
      (should (= c 3))
      (should (= d 4)))))

(ert-deftest vimp-test-read-key ()
  "Test `vimp-read-key'"
  :tags '(vimp util)
  (let ((unread-command-events '(?A)))
    (ert-info ("Prevent downcasing in `this-command-keys'")
      (should (eq (vimp-read-key) ?A))
      (should (equal (this-command-keys) "A")))))

(ert-deftest vimp-test-extract-count ()
  "Test `vimp-extract-count'"
  :tags '(vimp util)
  (vimp-test-buffer
    (ert-info ("Exact without count")
      (should (equal (vimp-extract-count "x")
                     (list nil 'vimp-delete-char "x" nil)))
      (should (equal (vimp-extract-count "g0")
                     (list nil 'vimp-beginning-of-visual-line "g0" nil))))

    (ert-info ("Exact with count")
      (should (equal (vimp-extract-count "420x")
                     (list 420 'vimp-delete-char "x" nil)))
      (should (equal (vimp-extract-count (vconcat "420" [M-right]))
                     (list 420 (key-binding [M-right]) (vconcat [M-right]) nil)))
      (should (equal (vimp-extract-count "2301g0")
                     (list 2301 'vimp-beginning-of-visual-line "g0" nil))))

    (ert-info ("Extra elements without count")
      (should (equal (vimp-extract-count "xAB")
                     (list nil 'vimp-delete-char "x" "AB")))
      (should (equal (vimp-extract-count "g0CD")
                     (list nil 'vimp-beginning-of-visual-line "g0" "CD"))))

    (ert-info ("Extra elements with count")
      (should (equal (vimp-extract-count "420xAB")
                     (list 420 'vimp-delete-char "x" "AB")))
      (should (equal (vimp-extract-count "2301g0CD")
                     (list 2301 'vimp-beginning-of-visual-line "g0" "CD"))))

    (ert-info ("Exact \"0\" count")
      (should (equal (vimp-extract-count "0")
                     (list nil 'vimp-digit-argument-or-vimp-beginning-of-line
                           "0" nil))))

    (ert-info ("Extra elements and \"0\"")
      (should (equal (vimp-extract-count "0XY")
                     (list nil 'vimp-digit-argument-or-vimp-beginning-of-line
                           "0" "XY"))))

    (ert-info ("Count only")
      (should-error (vimp-extract-count "1230")))

    (ert-info ("Unknown command")
      (should-error (vimp-extract-count "°"))
      (should-error (vimp-extract-count "12°")))))

(ert-deftest vimp-transform-vim-style-regexp ()
  "Test `vimp-transform-vim-style-regexp'"
  (dolist (repl '((?s . "[[:space:]]")
                  (?S . "[^[:space:]]")
                  (?d . "[[:digit:]]")
                  (?D . "[^[:digit:]]")
                  (?x . "[[:xdigit:]]")
                  (?X . "[^[:xdigit:]]")
                  (?o . "[0-7]")
                  (?O . "[^0-7]")
                  (?a . "[[:alpha:]]")
                  (?A . "[^[:alpha:]]")
                  (?l . "[a-z]")
                  (?L . "[^a-z]")
                  (?u . "[A-Z]")
                  (?U . "[^A-Z]")
                  (?y . "\\s")
                  (?Y . "\\S")
                  (?w . "\\w")
                  (?W . "\\W")))
    (ert-info ((format "Test transform from '\\%c' to '%s'"
                       (car repl) (cdr repl)))
      (should (equal (vimp-transform-vim-style-regexp
                      (concat "xxx\\"
                              (char-to-string (car repl))
                              "\\"
                              (char-to-string (car repl))
                              "\\\\"
                              (char-to-string (car repl))
                              "\\\\\\"
                              (char-to-string (car repl))
                              "yyy"))
                     (concat "xxx"
                             (cdr repl)
                             (cdr repl)
                             "\\\\"
                             (char-to-string (car repl))
                             "\\\\"
                             (cdr repl)
                             "yyy"))))))

;;; Advice

(ert-deftest vimp-test-eval-last-sexp ()
  "Test advised `vimp-last-sexp'"
  :tags '(vimp advice)
  (ert-info ("Normal state")
    (vimp-test-buffer
      "(+ 1 (+ 2 3[)])"
      ("1" (kbd "C-x C-e"))
      "(+ 1 (+ 2 35[)])"))
  (ert-info ("Insert state")
    (vimp-test-buffer
      "(+ 1 (+ 2 3[)])"
      ("i" (kbd "C-u") (kbd "C-x C-e") [escape])
      "(+ 1 (+ 2 3[3]))"))
  (ert-info ("Emacs state")
    (vimp-test-buffer
      "(+ 1 (+ 2 3[)])"
      ((kbd "C-z") (kbd "C-u") (kbd "C-x C-e"))
      "(+ 1 (+ 2 33[)])")))

;;; ESC

(ert-deftest vimp-test-esc-count ()
  "Test if prefix-argument is transfered for key sequences with meta-key"
  :tags '(vimp esc)
  (unless noninteractive
    (ert-info ("Test M-<right>")
      (vimp-test-buffer
        "[A]BC DEF GHI JKL MNO"
        ("3" (kbd "ESC <right>"))
        "ABC DEF GHI[ ]JKL MNO"))
    (ert-info ("Test shell-command")
      (vimp-test-buffer
        "[A]BC DEF GHI JKL MNO"
        ("1" (kbd "ESC !") "echo TEST" [return])
        "[T]EST\nABC DEF GHI JKL MNO"))))

(when (or vimp-tests-profiler vimp-tests-run)
  (vimp-tests-initialize))

(ert-deftest vimp-test-black-hole-register ()
  :tags '(vimp)
  (ert-info ("Test \"_ on delete word")
    (vimp-test-buffer
      "[E]vil vimp is awesome."
      ("dw\"_dwP")
      "Evil[ ]is awesome."))
  (ert-info ("Test \"_ on delete line")
    (vimp-test-buffer
      "[T]his line is a keeper!\nThis line is not."
      ("dd\"_ddP")
      "[T]his line is a keeper!"))
  (ert-info ("Test \"_ on delete region")
    (vimp-test-buffer
      "<This region is a keeper>!\nThis line is not."
      ("d\gg\"_dGP")
      "This region is a keepe[r]")))

(ert-deftest vimp-test-pasteable-macros ()
  "Test if we can yank and paste macros containing
                  <escape>"
  :tags '(vimp)
  (ert-info ("Execute yanked macro")
    (vimp-test-buffer
      "[i]foo\e"
      ("\"qd$@q\"qp"
       "fooifoo\e")))
  (ert-info ("Paste recorded marco")
    (vimp-test-buffer
      ""
      (vimp-set-register ?q (vconcat "ifoo" [escape]))
      ("@q\"qp")
      "fooifoo\e")))

(ert-deftest vimp-test-forward-symbol ()
  :tags '(vimp)
  (ert-info ("Test symbol deletion")
    (vimp-test-buffer
      "(test [t]his (hello there) with dao)"
      ("dao")
      "(test [(]hello there) with dao)"))
  (ert-info ("Test symbol motion")
    (vimp-test-buffer
      "(test[ ](hello there) with dao)"
      (should (eq 0 (forward-vimp-symbol 1)))
      "(test ([h]ello there) with dao)"
      (should (eq 0 (forward-vimp-symbol 1)))
      "(test (hello[ ]there) with dao)"))
  (ert-info ("Test dio on whitespace")
    (vimp-test-buffer
      "(test[ ]dio with whitespace)"
      ("dio")
      "(test[d]io with whitespace)"))
  (ert-info ("Test dao/dio with empty lines")
    (vimp-test-buffer
      "there are two lines in this file\n[\n]and some whitespace between them"
      ("dao")
      "there are two lines in this file\n[a]nd some whitespace between them")
    (vimp-test-buffer
      "here are another two lines\n[\n]with a blank line between them"
      ("dio")
      "here are another two lines\n[w]ith a blank line between them"))
  (ert-info ("Test dao/dio with empty lines and punctuation")
    (vimp-test-buffer
      "These two lines \n[\n]!have punctuation on them"
      ("dao")
      "These two lines \n[!]have punctuation on them")))

(provide 'vimp-tests)

;;; vimp-tests.el ends here
