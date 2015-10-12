;;; vimp-core.el --- Core functionality
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

;;; Commentary:

;; Evil is defined as a globalized minor mode, enabled with the toggle
;; function `vimp-mode'.  This in turn enables `vimp-local-mode' in
;; every buffer, which sets up the buffer's state.
;;
;; Each state has its own keymaps, and these keymaps have status as
;; "emulation keymaps" with priority over regular keymaps.  Emacs
;; maintains the following keymap hierarchy (highest priority first):
;;
;;     * Overriding keymaps/overlay keymaps...
;;     * Emulation mode keymaps...
;;       - Evil keymaps...
;;     * Minor mode keymaps...
;;     * Local keymap (`local-set-key')
;;     * Global keymap (`global-set-key')
;;
;; Within this hierarchy, Evil arranges the keymaps for the current
;; state as shown below:
;;
;;     * Intercept keymaps...
;;     * Local state keymap
;;     * Auxiliary keymaps...
;;     * Overriding keymaps...
;;     * Global state keymap
;;     * Keymaps for other states...
;;
;; These keymaps are listed in `vimp-mode-map-alist', which is listed
;; in `emulation-mode-map-alist'.
;;
;; Most of the key bindings for a state are stored in its global
;; keymap, which has a name such as `vimp-normal-state-map'.  (See the
;; file vimp-maps.el, which contains all the default key bindings.)
;; A state also has a local keymap (`vimp-normal-state-local-map'),
;; which may contain user customizations for the current buffer.
;; Furthermore, any Emacs mode may be assigned state bindings of its
;; own by passing the mode's keymap to the function `vimp-define-key'.
;; These mode-specific bindings are ultimately stored in so-called
;; auxiliary keymaps, which are sandwiched between the local keymap
;; and the global keymap.  Finally, the state may also activate the
;; keymaps of other states (e.g., Normal state inherits bindings
;; from Motion state).
;;
;; For integration purposes, a regular Emacs keymap may be "elevated"
;; to emulation status by passing it to `vimp-make-intercept-map' or
;; `vimp-make-overriding-map'.  An "intercept" keymap has priority over
;; all other Evil keymaps.  (Evil uses this facility when debugging and
;; for handling the "ESC" key in the terminal.) More common is the
;; "overriding" keymap, which only has priority over the global state
;; keymap.  (This is useful for adapting key-heavy modes such as Dired,
;; where all but a few keys should be left as-is and should not be
;; shadowed by Evil's default bindings.)
;;
;; States are defined with the macro `vimp-define-state', which
;; creates a command for switching to the state.  This command,
;; for example `vimp-normal-state' for Normal state, performs
;; the following tasks:
;;
;;     * Setting `vimp-state' to the new state.
;;     * Refreshing the keymaps in `vimp-mode-map-alist'.
;;     * Updating the mode line.
;;       - Normal state depends on `vimp-normal-state-tag'.
;;     * Adjusting the cursor's appearance.
;;       - Normal state depends on `vimp-normal-state-cursor'.
;;     * Displaying a message in the echo area.
;;       - Normal state depends on `vimp-normal-state-message'.
;;     * Running hooks.
;;       - Normal state runs `vimp-normal-state-entry-hook' when
;;         entering, and `vimp-normal-state-exit-hook' when exiting.
;;
;; The various properties of a state can be accessed through their
;; respective variables, or by passing a keyword and the state's name
;; to the `vimp-state-property' function.  Evil defines the states
;; Normal state ("normal"), Insert state ("insert"), Visual state
;; ("visual"), Replace state ("replace"), Operator-Pending state
;; ("operator"), Motion state ("motion") and Emacs state ("emacs").

(require 'vimp-common)

;;; Code:

(declare-function vimp-emacs-state-p "vimp-states")
(declare-function vimp-ex-p "vimp-ex")
(defvar vimp-mode-buffers)

(define-minor-mode vimp-local-mode
  "Minor mode for setting up Evil in a single buffer."
  :init-value nil
  (cond
   ((vimp-disabled-buffer-p))
   (vimp-local-mode
    (setq emulation-mode-map-alists
          (vimp-concat-lists '(vimp-mode-map-alist)
                             emulation-mode-map-alists))
    (vimp-initialize-local-keymaps)
    ;; restore the proper value of `major-mode' in Fundamental buffers
    (when (eq major-mode 'turn-on-vimp-mode)
      (setq major-mode 'fundamental-mode))
    ;; The initial state is usually setup by `vimp-initialize' when
    ;; the major-mode in a buffer changes. This preliminary
    ;; initialization is only for the case when `vimp-local-mode' is
    ;; called directly for the first time in a buffer.
    (unless vimp-state (vimp-initialize-state))
    (add-hook 'input-method-activate-hook 'vimp-activate-input-method t t)
    (add-hook 'input-method-deactivate-hook 'vimp-deactivate-input-method t t)
    (add-hook 'activate-mark-hook 'vimp-visual-activate-hook nil t)
    (add-hook 'pre-command-hook 'vimp-repeat-pre-hook)
    (add-hook 'pre-command-hook 'vimp-jump-hook nil t)
    (add-hook 'post-command-hook 'vimp-repeat-post-hook))
   (t
    (vimp-refresh-mode-line)
    (remove-hook 'pre-command-hook 'vimp-jump-hook t)
    (remove-hook 'activate-mark-hook 'vimp-visual-activate-hook t)
    (remove-hook 'input-method-activate-hook 'vimp-activate-input-method t)
    (remove-hook 'input-method-deactivate-hook 'vimp-deactivate-input-method t)
    (vimp-change-state nil))))

;; Make the variable permanent local.  This is particular useful in
;; conjunction with nXhtml/mumamo because mumamo does not touch these
;; variables.
(put 'vimp-local-mode 'permanent-local t)

(defun turn-on-vimp-mode (&optional arg)
  "Turn on Evil in the current buffer."
  (interactive)
  (vimp-local-mode (or arg 1)))

(defun turn-off-vimp-mode (&optional arg)
  "Turn off Evil in the current buffer."
  (interactive)
  (vimp-local-mode (or arg -1)))

;; The function `vimp-initialize' should only be used to initialize
;; `vimp-local-mode' from the globalized minor-mode `vimp-mode'. It is
;; called whenever vimp is enabled in a buffer for the first time or
;; when vimp is active and the major-mode of the buffer changes. In
;; addition to enabling `vimp-local-mode' it also sets the initial
;; vimp-state according to the major-mode.
(defun vimp-initialize ()
  "Enable Evil in the current buffer, if appropriate.
To enable Evil globally, do (vimp-mode 1)."
  ;; TODO: option for enabling vi keys in the minibuffer
  (unless (minibufferp)
    (vimp-local-mode 1)
    (vimp-initialize-state)))

;; No hooks are run in Fundamental buffers, so other measures are
;; necessary to initialize Evil in these buffers. When Evil is
;; enabled globally, the default value of `major-mode' is set to
;; `turn-on-vimp-mode', so that Evil is enabled in Fundamental
;; buffers as well. Then, the buffer-local value of `major-mode' is
;; changed back to `fundamental-mode'. (Since the `vimp-mode' function
;; is created by a macro, we use `defadvice' to augment it.)
(defadvice vimp-mode (after start-vimp activate)
  "Enable Evil in Fundamental mode."
  (if vimp-mode
      (progn
        (when (eq (default-value 'major-mode) 'fundamental-mode)
          ;; changed back by `vimp-local-mode'
          (setq-default major-mode 'turn-on-vimp-mode))
        (ad-enable-regexp "^vimp")
        (ad-activate-regexp "^vimp")
        (with-no-warnings (vimp-esc-mode 1)))
    (when (eq (default-value 'major-mode) 'turn-on-vimp-mode)
      (setq-default major-mode 'fundamental-mode))
    (ad-disable-regexp "^vimp")
    (ad-update-regexp "^vimp")
    (with-no-warnings (vimp-esc-mode -1))))

(defun vimp-change-state (state &optional message)
  "Change the state to STATE.
If STATE is nil, disable all states."
  (let ((func (vimp-state-property (or state vimp-state) :toggle)))
    (when (and (functionp func)
               (or message (not (eq state vimp-state))))
      (funcall func (if state (and message 1) -1)))))

(defmacro vimp-save-state (&rest body)
  "Save the current state; execute BODY; restore the state."
  (declare (indent defun)
           (debug t))
  `(let* ((vimp-state vimp-state)
          (vimp-previous-state vimp-previous-state)
          (vimp-previous-state-alist (copy-tree vimp-previous-state-alist))
          (vimp-next-state vimp-next-state)
          (old-state vimp-state)
          (inhibit-quit t)
          (buf (current-buffer)))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (vimp-change-state old-state))))))

(defmacro vimp-with-state (state &rest body)
  "Change to STATE and execute BODY without refreshing the display.
Restore the previous state afterwards."
  (declare (indent defun)
           (debug t))
  `(vimp-without-display
     (vimp-save-state
       (vimp-change-state ',state)
       ,@body)))

(defun vimp-initializing-p (&optional buffer)
  "Whether Evil is in the process of being initialized."
  (memq (or buffer (current-buffer)) vimp-mode-buffers))

(defun vimp-initialize-state (&optional state buffer)
  "Set up the initial state for BUFFER.
BUFFER defaults to the current buffer.
Uses STATE if specified, or calls `vimp-initial-state-for-buffer'.
See also `vimp-set-initial-state'."
  (with-current-buffer (or buffer (current-buffer))
    (if state (vimp-change-state state)
      (vimp-change-to-initial-state buffer))))
(put 'vimp-initialize-state 'permanent-local-hook t)

(defun vimp-initial-state-for-buffer-name (&optional name default)
  "Return the initial Evil state to use for a buffer with name NAME.
Matches the name against the regular expressions in
`vimp-buffer-regexps'. If none matches, returns DEFAULT."
  (let ((name (if (stringp name) name (buffer-name name)))
        regexp state)
    (when (stringp name)
      (catch 'done
        (dolist (entry vimp-buffer-regexps default)
          (setq regexp (car entry)
                state (cdr entry))
          (when (string-match regexp name)
            (throw 'done state)))))))

(defun vimp-disabled-buffer-p (&optional buffer)
  "Whether Evil should be disabled in BUFFER."
  (null (vimp-initial-state-for-buffer-name buffer 'undefined)))

(defun vimp-initial-state-for-buffer (&optional buffer default)
  "Return the initial Evil state to use for BUFFER.
BUFFER defaults to the current buffer. Returns DEFAULT
if no initial state is associated with BUFFER.
See also `vimp-initial-state'."
  (with-current-buffer (or buffer (current-buffer))
    (or (vimp-initial-state-for-buffer-name (buffer-name))
        (catch 'done
          (dolist (mode minor-mode-map-alist)
            (setq mode (car-safe mode))
            (when (and (boundp mode) (symbol-value mode))
              (when (setq mode (vimp-initial-state mode))
                (throw 'done mode)))))
        (vimp-initial-state major-mode)
        default)))

(defun vimp-initial-state (mode &optional default)
  "Return the Evil state to use for MODE.
Returns DEFAULT if no initial state is associated with MODE.
The initial state for a mode can be set with
`vimp-set-initial-state'."
  (let (state modes)
    (catch 'done
      (dolist (entry (vimp-state-property t :modes) default)
        (setq state (car entry)
              modes (symbol-value (cdr entry)))
        (when (memq mode modes)
          (throw 'done state))))))

(defun vimp-set-initial-state (mode state)
  "Set the initial state for MODE to STATE.
This is the state the buffer comes up in."
  (dolist (modes (vimp-state-property t :modes))
    (setq modes (cdr-safe modes))
    (set modes (delq mode (symbol-value modes))))
  (when state
    (add-to-list (vimp-state-property state :modes) mode)))

(vimp-define-command vimp-change-to-initial-state
  (&optional buffer message)
  "Change the state of BUFFER to its initial state.
This is the state the buffer came up in. If Evil is not activated
then this function does nothing."
  :keep-visual t
  :suppress-operator t
  (with-current-buffer (or buffer (current-buffer))
    (when vimp-local-mode
      (vimp-change-state (vimp-initial-state-for-buffer
                          buffer (or vimp-default-state 'normal))
                         message))))

(vimp-define-command vimp-change-to-previous-state
  (&optional buffer message)
  "Change the state of BUFFER to its previous state."
  :keep-visual t
  :repeat abort
  :suppress-operator t
  (with-current-buffer (or buffer (current-buffer))
    (let ((prev-state vimp-previous-state)
          (prev-prev-state (cdr-safe (assoc vimp-previous-state
                                            vimp-previous-state-alist))))
      (vimp-change-state nil)
      (when prev-prev-state
        (setq vimp-previous-state prev-prev-state))
      (vimp-change-state (or prev-state vimp-default-state 'normal)
                         message))))

;; When a buffer is created in a low-level way, it is invisible to
;; Evil (as well as other globalized minor modes) because no hooks are
;; run. This is appropriate since many buffers are used for throwaway
;; purposes. Passing the buffer to `set-window-buffer' indicates
;; otherwise, though, so advise this function to initialize Evil.
(defadvice set-window-buffer (before vimp)
  "Initialize Evil in the displayed buffer."
  (when vimp-mode
    (when (get-buffer (ad-get-arg 1))
      (with-current-buffer (ad-get-arg 1)
        (unless vimp-local-mode
          (vimp-local-mode 1))))))

;; Refresh cursor color.
;; Cursor color can only be set for each frame but not for each buffer.
(add-hook 'window-configuration-change-hook 'vimp-refresh-cursor)
(defadvice select-window (after vimp activate)
  (vimp-refresh-cursor))

(defun vimp-generate-mode-line-tag (&optional state)
  "Generate the vimp mode-line tag for STATE."
  (let ((tag (vimp-state-property state :tag t)))
    ;; prepare mode-line: add tooltip
    (if (stringp tag)
        (propertize tag
                    'help-echo (vimp-state-property state :name)
                    'mouse-face 'mode-line-highlight)
      tag)))

(defun vimp-refresh-mode-line (&optional state)
  "Refresh mode line tag."
  (when (listp mode-line-format)
    (setq vimp-mode-line-tag (vimp-generate-mode-line-tag state))
    ;; refresh mode line data structure
    ;; first remove vimp from mode-line
    (setq mode-line-format (delq 'vimp-mode-line-tag mode-line-format))
    (let ((mlpos mode-line-format)
          pred which where)
      ;; determine before/after which symbol the tag should be placed
      (cond
       ((eq vimp-mode-line-format 'before)
        (setq where 'after which 'mode-line-position))
       ((eq vimp-mode-line-format 'after)
        (setq where 'after which 'mode-line-modes))
       ((consp vimp-mode-line-format)
        (setq where (car vimp-mode-line-format)
              which (cdr vimp-mode-line-format))))
      ;; find the cons-cell of the symbol before/after which the tag
      ;; should be placed
      (while (and mlpos
                  (let ((sym (or (car-safe (car mlpos)) (car mlpos))))
                    (not (eq which sym))))
        (setq pred mlpos
              mlpos (cdr mlpos)))
      ;; put vimp tag at the right position in the mode line
      (cond
       ((not mlpos)) ;; position not found, so do not add the tag
       ((eq where 'before)
        (if pred
            (setcdr pred (cons 'vimp-mode-line-tag mlpos))
          (setq mode-line-format
                (cons 'vimp-mode-line-tag mode-line-format))))
       ((eq where 'after)
        (setcdr mlpos (cons 'vimp-mode-line-tag (cdr mlpos)))))
      (force-mode-line-update))))

;; input methods should be disabled in non-insertion states
(defun vimp-activate-input-method ()
  "Enable input method in states with :input-method non-nil."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (when (and vimp-local-mode vimp-state)
      (setq vimp-input-method current-input-method)
      (unless (vimp-state-property vimp-state :input-method)
        (deactivate-input-method)))))
(put 'vimp-activate-input-method 'permanent-local-hook t)

(defun vimp-deactivate-input-method ()
  "Disable input method in all states."
  (let (input-method-activate-hook
        input-method-deactivate-hook)
    (when (and vimp-local-mode vimp-state)
      (setq vimp-input-method nil))))
(put 'vimp-deactivate-input-method 'permanent-local-hook t)

(defmacro vimp-without-input-method-hooks (&rest body)
  "Execute body with vimp's activate/deactivate-input-method hooks deactivated.

This allows input methods to be used in normal-state."
  `(unwind-protect
       (progn
         (remove-hook 'input-method-activate-hook 'vimp-activate-input-method t)
         (remove-hook 'input-method-deactivate-hook
                      'vimp-deactivate-input-method t)
         ,@body)
     (progn
       (add-hook 'input-method-activate-hook 'vimp-activate-input-method nil t)
       (add-hook 'input-method-deactivate-hook
                 'vimp-deactivate-input-method nil t))))

(defadvice toggle-input-method (around vimp)
  "Refresh `vimp-input-method'."
  (cond
   ((not vimp-local-mode)
    ad-do-it)
   ((vimp-state-property vimp-state :input-method)
    ad-do-it)
   (t
    (let ((current-input-method vimp-input-method))
      ad-do-it))))

;; Local keymaps are implemented using buffer-local variables.
;; However, unless a buffer-local value already exists,
;; `define-key' acts on the variable's default (global) value.
;; So we need to initialize the variable whenever we enter a
;; new buffer or when the buffer-local values are reset.
(defun vimp-initialize-local-keymaps ()
  "Initialize a buffer-local value for local keymaps as necessary.
The initial value is that of `make-sparse-keymap'."
  (dolist (entry vimp-local-keymaps-alist)
    (let ((mode (car entry))
          (map  (cdr entry)))
      (unless (and (keymapp (symbol-value map))
                   (assq map (buffer-local-variables)))
        (set map (make-sparse-keymap))))))

(defun vimp-make-overriding-map (keymap &optional state copy)
  "Give KEYMAP precedence over the global keymap of STATE.
The keymap will have lower precedence than custom STATE bindings.
If STATE is nil, give it precedence over all states.
If COPY is t, create a copy of KEYMAP and give that
higher precedence. See also `vimp-make-intercept-map'."
  (let ((key [override-state]))
    (if (not copy)
        (define-key keymap key (or state 'all))
      (unless (keymapp copy)
        (setq copy (assq-delete-all 'menu-bar (copy-keymap keymap))))
      (define-key copy key (or state 'all))
      (define-key keymap key copy))))

(defun vimp-make-intercept-map (keymap &optional state)
  "Give KEYMAP precedence over all Evil keymaps in STATE.
If STATE is nil, give it precedence over all states.
See also `vimp-make-overriding-map'."
  (let ((key [intercept-state]))
    (define-key keymap key (or state 'all))))

(defmacro vimp-define-keymap (keymap doc &rest body)
  "Define a keymap KEYMAP listed in `vimp-mode-map-alist'.
That means it will have precedence over regular keymaps.

DOC is the documentation for the variable. BODY, if specified,
is executed after toggling the mode. Optional keyword arguments
may be specified before the body code:

:mode VAR       Mode variable. If unspecified, the variable
                is based on the keymap name.
:local BOOLEAN  Whether the keymap should be buffer-local, that is,
                reinitialized for each buffer.
:func BOOLEAN   Create a toggle function even if BODY is empty.

\(fn KEYMAP DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let ((func t)
        arg intercept key local mode overriding)
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :mode)
        (setq mode arg))
       ((eq key :local)
        (setq local arg))
       ((eq key :func)
        (setq func arg))
       ((eq key :intercept)
        (setq intercept arg))
       ((eq key :overriding)
        (setq overriding arg))))
    (setq mode (or mode
                   (intern (replace-regexp-in-string
                            "\\(?:-\\(?:mode-\\)?\\(?:key\\)?map\\)?$"
                            "-mode"
                            (symbol-name keymap)))))
    `(progn
       (defvar ,keymap ,(unless local '(make-sparse-keymap)))
       (unless (get ',keymap 'variable-documentation)
         (put ',keymap 'variable-documentation ,doc))
       (defvar ,mode nil)
       (unless (get ',mode 'variable-documentation)
         (put ',mode 'variable-documentation ,doc))
       (make-variable-buffer-local ',mode)
       (put ',mode 'permanent-local t)
       (when ,intercept
         (vimp-make-intercept-map ,keymap))
       (when ,overriding
         (vimp-make-overriding-map ,keymap))
       ,@(if local
             `((make-variable-buffer-local ',keymap)
               (put ',keymap 'permanent-local t)
               (vimp-add-to-alist 'vimp-local-keymaps-alist
                                  ',mode ',keymap))
           `((vimp-add-to-alist 'vimp-global-keymaps-alist
                                ',mode ',keymap)
             (vimp-add-to-alist 'vimp-mode-map-alist
                                ',mode ,keymap)))
       ,(when (or body func)
          `(defun ,mode (&optional arg)
             ,@(when doc `(,doc))
             (interactive)
             (cond
              ((numberp arg)
               (setq ,mode (> arg 0)))
              (t
               (setq ,mode (not ,mode))))
             ,@body))
       ',keymap)))

;; The ESC -> escape translation code has been provided by Stefan
;; Monnier in the discussion of GNU Emacs bug #13793.
(defun vimp-esc-mode (&optional arg)
  "Toggle interception of \\e (escape).
Enable with positive ARG and disable with negative ARG.

When enabled, `vimp-esc-mode' modifies the entry of \\e in
`input-decode-map'. If such an event arrives, it is translated to
a plain 'escape event if no further event occurs within
`vimp-esc-delay' seconds. Otherwise no translation happens and
the ESC prefix map (i.e. the map originally bound to \\e in
`input-decode-map`) is returned."
  (cond
   ((or (null arg) (eq arg 0))
    (vimp-esc-mode (if vimp-esc-mode -1 +1)))
   ((> arg 0)
    (unless vimp-esc-mode
      (setq vimp-esc-mode t)
      (add-hook 'after-make-frame-functions #'vimp-init-esc)
      (mapc #'vimp-init-esc (frame-list))))
   ((< arg 0)
    (when vimp-esc-mode
      (remove-hook 'after-make-frame-functions #'vimp-init-esc)
      (mapc #'vimp-deinit-esc (frame-list))
      (setq vimp-esc-mode nil)))))

(defun vimp-init-esc (frame)
  "Update `input-decode-map' in terminal."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (and
             (or (eq vimp-intercept-esc 'always)
                 (and vimp-intercept-esc
                      (eq (terminal-live-p term) t))) ; only patch tty
             (not (terminal-parameter term 'vimp-esc-map)))
        (let ((vimp-esc-map (lookup-key input-decode-map [?\e])))
          (set-terminal-parameter term 'vimp-esc-map vimp-esc-map)
          (define-key input-decode-map [?\e]
            `(menu-item "" ,vimp-esc-map :filter ,#'vimp-esc)))))))

(defun vimp-deinit-esc (frame)
  "Restore `input-decode-map' in terminal."
  (with-selected-frame frame
    (let ((term (frame-terminal frame)))
      (when (terminal-live-p term)
        (let ((vimp-esc-map (terminal-parameter term 'vimp-esc-map)))
          (when vimp-esc-map
            (define-key input-decode-map [?\e] vimp-esc-map)
            (set-terminal-parameter term 'vimp-esc-map nil)))))))

(defun vimp-esc (map)
  "Translate \\e to 'escape if no further event arrives.
This function is used to translate a \\e event either to 'escape
or to the standard ESC prefix translation map. If \\e arrives,
this function waits for `vimp-esc-delay' seconds for another
event. If no other event arrives, the event is translated to
'escape, otherwise it is translated to the standard ESC prefix
map stored in `input-decode-map'. If `vimp-inhibit-esc' is
non-nil or if vimp is in emacs state, the event is always
translated to the ESC prefix.

The translation to 'escape happens only if the current command
has indeed been triggered by \\e. In other words, this will only
happen when the keymap is accessed from `read-key-sequence'. In
particular, if it is access from `define-key' the returned
mapping will always be the ESC prefix map."
  (if (and (not vimp-inhibit-esc)
           (or vimp-local-mode (vimp-ex-p))
           (not (vimp-emacs-state-p))
           (let ((keys (this-single-command-keys)))
             (and (> (length keys) 0)
                  (= (aref keys (1- (length keys))) ?\e)))
           (sit-for vimp-esc-delay))
      (prog1 [escape]
        (when defining-kbd-macro
          (end-kbd-macro)
          (setq last-kbd-macro (vconcat last-kbd-macro [escape]))
          (start-kbd-macro t t)))
    map))

(defun vimp-state-p (sym)
  "Whether SYM is the name of a state."
  (assq sym vimp-state-properties))

(defun vimp-state-keymaps (state &rest excluded)
  "Return a keymap alist of keymaps activated by STATE.
If STATE references other states in its :enable property,
these states are recursively processed and added to the list.
\(The EXCLUDED argument is an internal safeguard against
infinite recursion, keeping track of processed states.)"
  (let* ((state (or state vimp-state))
         (enable (vimp-state-property state :enable))
         (map (cons
               (vimp-state-property state :mode)
               (vimp-state-property state :keymap t)))
         (local-map (cons
                     (vimp-state-property state :local)
                     (vimp-state-property state :local-keymap t)))
         (aux-maps (vimp-state-auxiliary-keymaps state))
         (overriding-maps
          (vimp-state-overriding-keymaps state))
         (intercept-maps
          (vimp-state-intercept-keymaps state))
         (result `(,intercept-maps))
         (remove-duplicates (null excluded)))
    (unless (memq state enable)
      (setq enable (cons state enable)))
    ;; process STATE's :enable property
    (dolist (entry enable)
      (cond
       ((memq entry excluded))
       ;; the keymaps for STATE
       ((eq entry state)
        (setq result `(,@result
                       (,local-map)
                       ,aux-maps
                       ,overriding-maps
                       (,map)))
        (push state excluded))
       ;; the keymaps for another state: call `vimp-state-keymaps'
       ;; recursively, but keep track of processed states
       ((vimp-state-p entry)
        (setq result `(,@result
                       ,(apply #'vimp-state-keymaps entry excluded))))
       ;; a single keymap
       ((or (keymapp entry)
            (and (keymapp (symbol-value entry))
                 (setq entry (symbol-value entry)))
            (setq entry (vimp-keymap-for-mode entry)))
        (setq result `(,@result
                       ((,(vimp-mode-for-keymap entry t) .
                         ,entry)))))))
    ;; postpone the expensive filtering of duplicates to the top level
    (if remove-duplicates
        (apply #'vimp-concat-keymap-alists result)
      (apply #'append result))))

(defun vimp-normalize-keymaps (&optional state)
  "Create a buffer-local value for `vimp-mode-map-alist'.
This is a keymap alist, determined by the current state
\(or by STATE if specified)."
  (let ((state (or state vimp-state))
        (excluded '(nil t))
        map mode temp)
    ;; initialize buffer-local keymaps as necessary
    (vimp-initialize-local-keymaps)
    ;; deactivate keymaps of previous state
    (dolist (entry vimp-mode-map-alist)
      (setq mode (car-safe entry)
            map (cdr-safe entry))
      ;; don't deactivate overriding keymaps;
      ;; they are toggled by their associated mode
      (if (or (memq mode excluded)
              (vimp-intercept-keymap-p map)
              (vimp-overriding-keymap-p map)
              (vimp-auxiliary-keymap-p map))
          (push mode excluded)
        (when (and (fboundp mode) (symbol-value mode))
          (funcall mode -1))
        (set mode nil)))
    (setq vimp-mode-map-alist nil)
    ;; activate keymaps of current state
    (when state
      (setq temp (vimp-state-keymaps state))
      (dolist (entry temp)
        (setq mode (car entry)
              map (cdr entry))
        (unless (and (boundp mode) (symbol-value mode))
          (when (fboundp mode)
            (funcall mode 1))
          (set mode t))
        ;; refresh the keymap in case it has changed
        ;; (e.g., `vimp-operator-shortcut-map' is
        ;; reset on toggling)
        (if (or (memq mode excluded)
                (vimp-intercept-keymap-p map)
                (vimp-overriding-keymap-p map)
                (vimp-auxiliary-keymap-p map))
            (push mode excluded)
          (setcdr entry (or (vimp-keymap-for-mode mode) map))))
      ;; update `vimp-mode-map-alist'
      (setq vimp-mode-map-alist temp))))

(defun vimp-mode-for-keymap (keymap &optional default)
  "Return the minor mode associated with KEYMAP.
Returns DEFAULT if no mode is found.
See also `vimp-keymap-for-mode'."
  (let ((map (if (keymapp keymap) keymap (symbol-value keymap)))
        (var (when (symbolp keymap) keymap)))
    ;; Check Evil variables first for speed purposes.
    ;; If all else fails, check `minor-mode-map-alist'.
    (or (when var
          (or (car (rassq var vimp-global-keymaps-alist))
              (car (rassq var vimp-local-keymaps-alist))))
        (car (rassq map (mapcar #'(lambda (e)
                                    ;; from (MODE-VAR . MAP-VAR)
                                    ;; to (MODE-VAR . MAP)
                                    (cons (car-safe e)
                                          (symbol-value (cdr-safe e))))
                                (append vimp-global-keymaps-alist
                                        vimp-local-keymaps-alist))))
        (car (rassq map minor-mode-map-alist))
        default)))

(defun vimp-keymap-for-mode (mode &optional variable)
  "Return the keymap associated with MODE.
Return the keymap variable if VARIABLE is non-nil.
See also `vimp-mode-for-keymap'."
  (let* ((var (or (cdr (assq mode vimp-global-keymaps-alist))
                  (cdr (assq mode vimp-local-keymaps-alist))))
         (map (or (symbol-value var)
                  (cdr (assq mode minor-mode-map-alist)))))
    (if variable var map)))

(defun vimp-state-auxiliary-keymaps (state)
  "Return a keymap alist of auxiliary keymaps for STATE."
  (let ((state (or state vimp-state))
        aux result)
    (dolist (map (current-active-maps) result)
      (when (setq aux (vimp-get-auxiliary-keymap map state))
        (push (cons (vimp-mode-for-keymap map t) aux) result)))
    (nreverse result)))

(defun vimp-state-overriding-keymaps (&optional state)
  "Return a keymap alist of overriding keymaps for STATE."
  (let* ((state (or state vimp-state))
         result)
    (dolist (map (current-active-maps))
      (when (setq map (vimp-overriding-keymap-p map state))
        (push (cons (vimp-mode-for-keymap map t) map) result)))
    (nreverse result)))

(defun vimp-state-intercept-keymaps (&optional state)
  "Return a keymap alist of intercept keymaps for STATE."
  (let* ((state (or state vimp-state))
         result)
    (dolist (map (current-active-maps))
      (when (setq map (vimp-intercept-keymap-p map state))
        (push (cons (vimp-mode-for-keymap map t) map) result)))
    (setq result (nreverse result))
    result))

(defun vimp-set-auxiliary-keymap (map state &optional aux)
  "Set the auxiliary keymap for MAP in STATE to AUX.
If AUX is nil, create a new auxiliary keymap."
  (unless (keymapp aux)
    (setq aux (make-sparse-keymap)))
  (unless (vimp-auxiliary-keymap-p aux)
    (vimp-set-keymap-prompt
     aux (format "Auxiliary keymap for %s"
                 (or (vimp-state-property state :name)
                     (format "%s state" state)))))
  (define-key map
    (vconcat (list (intern (format "%s-state" state)))) aux)
  aux)
(put 'vimp-set-auxiliary-keymap 'lisp-indent-function 'defun)

(defun vimp-get-auxiliary-keymap (map state &optional create)
  "Get the auxiliary keymap for MAP in STATE.
If CREATE is non-nil, create an auxiliary keymap
if MAP does not have one."
  (when state
    (let* ((key (vconcat (list (intern (format "%s-state" state)))))
           (aux (if state (lookup-key map key) map)))
      (cond
       ((vimp-auxiliary-keymap-p aux)
        aux)
       (create
        (vimp-set-auxiliary-keymap map state))))))

(defun vimp-auxiliary-keymap-p (map)
  "Whether MAP is an auxiliary keymap."
  (and (keymapp map)
       (string-match "Auxiliary keymap"
                     (or (keymap-prompt map) "")) t))

(defun vimp-intercept-keymap-p (map &optional state)
  "Whether MAP is an intercept keymap for STATE.
If STATE is nil, it means any state."
  (let ((entry (and (keymapp map)
                    (lookup-key map [intercept-state]))))
    (cond
     ((null entry)
      nil)
     ((null state)
      map)
     ((eq entry state)
      map)
     ((eq entry 'all)
      map))))

(defun vimp-overriding-keymap-p (map &optional state)
  "Whether MAP is an overriding keymap for STATE.
If STATE is nil, it means any state."
  (let ((entry (and (keymapp map)
                    (lookup-key map [override-state]))))
    (cond
     ((null entry)
      nil)
     ((keymapp entry)
      (vimp-overriding-keymap-p entry state))
     ((null state)
      map)
     ((eq entry state)
      map)
     ((eq entry 'all)
      map))))

(defun vimp-intercept-keymap-state (map)
  "Return the state for the intercept keymap MAP.
A return value of t means all states."
  (let ((state (lookup-key map [intercept-state] map)))
    (cond
     ((keymapp state)
      (vimp-intercept-keymap-state state))
     ((eq state 'all)
      t)
     (t
      state))))

(defun vimp-overriding-keymap-state (map)
  "Return the state for the overriding keymap MAP.
A return value of t means all states."
  (let ((state (lookup-key map [override-state] map)))
    (cond
     ((keymapp state)
      (vimp-overriding-keymap-state state))
     ((eq state 'all)
      t)
     (t
      state))))

(defmacro vimp-define-key (state keymap key def &rest bindings)
  "Create a STATE binding from KEY to DEF for KEYMAP.
STATE is one of `normal', `insert', `visual', `replace',
`operator', `motion' and `emacs'. The remaining arguments
are like those of `define-key'. For example:

    (vimp-define-key 'normal foo-map \"a\" 'bar)

This creates a binding from \"a\" to `bar' in Normal state,
which is active whenever `foo-map' is active. It is possible
to specify multiple bindings at once:

    (vimp-define-key 'normal foo-map
      \"a\" 'bar
      \"b\" 'foo)

If foo-map has not been initialized yet, this macro adds an entry
to `after-load-functions', delaying execution as necessary."
  (declare (indent defun))
  `(vimp-delay ',(if (symbolp keymap)
                     `(and (boundp ',keymap) (keymapp ,keymap))
                   `(keymapp ,keymap))
       '(let* ((state ,state) (keymap ,keymap) (key ,key) (def ,def)
               (bindings (list ,@bindings)) aux)
          (if state
              (setq aux (vimp-get-auxiliary-keymap keymap state t))
            (setq aux keymap))
          (while key
            (define-key aux key def)
            (setq key (pop bindings)
                  def (pop bindings)))
          ;; ensure the prompt string comes first
          (vimp-set-keymap-prompt aux (keymap-prompt aux)))
     'after-load-functions t nil
     (format "vimp-define-key-in-%s"
             ',(if (symbolp keymap) keymap 'keymap))))
(defalias 'vimp-declare-key 'vimp-define-key)

(defmacro vimp-add-hjkl-bindings (keymap &optional state &rest bindings)
  "Add \"h\", \"j\", \"k\", \"l\" bindings to KEYMAP in STATE.
Add additional BINDINGS if specified."
  (declare (indent defun))
  `(vimp-define-key ,state ,keymap
     "h" (lookup-key vimp-motion-state-map "h")
     "j" (lookup-key vimp-motion-state-map "j")
     "k" (lookup-key vimp-motion-state-map "k")
     "l" (lookup-key vimp-motion-state-map "l")
     ":" (lookup-key vimp-motion-state-map ":")
     ,@bindings))

;; may be useful for programmatic purposes
(defun vimp-global-set-key (state key def)
  "Bind KEY to DEF in STATE."
  (define-key (vimp-state-property state :keymap t) key def))

(defun vimp-local-set-key (state key def)
  "Bind KEY to DEF in STATE in the current buffer."
  (define-key (vimp-state-property state :local-keymap t) key def))

;; Advise these functions as they may activate an overriding keymap or
;; a keymap with state bindings; if so, refresh `vimp-mode-map-alist'.
(defadvice use-global-map (after vimp activate)
  "Refresh Evil keymaps."
  (vimp-normalize-keymaps))

(defadvice use-local-map (after vimp activate)
  "Refresh Evil keymaps."
  (vimp-normalize-keymaps))

(defmacro vimp-define-state (state doc &rest body)
  "Define an Evil state STATE.
DOC is a general description and shows up in all docstrings;
the first line of the string should be the full name of the state.
Then follows one or more optional keywords:

:tag STRING             Mode line indicator.
:message STRING         Echo area message when changing to STATE.
:cursor SPEC            Cursor to use in STATE.
:entry-hook LIST        Hooks run when changing to STATE.
:exit-hook LIST         Hooks run when changing from STATE.
:enable LIST            List of other states and modes enabled by STATE.
:suppress-keymap FLAG   If FLAG is non-nil, makes `vimp-suppress-map'
                        the parent of the global map of STATE,
                        effectively disabling bindings to
                        `self-insert-command'.

Following the keywords is optional code to be executed each time
the state is enabled or disabled. For example:

    (vimp-define-state test
      \"Test state.\"
      :tag \"<T> \"
      (setq test-var t))

The global keymap of this state will be `vimp-test-state-map',
the local keymap will be `vimp-test-state-local-map', and so on.

\(fn STATE DOC [[KEY VAL]...] BODY...)"
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest [keywordp sexp]]
                           def-body)))
  (let* ((name (and (string-match "^\\(.+\\)\\(\\(?:.\\|\n\\)*\\)" doc)
                    (match-string 1 doc)))
         (doc (match-string 2 doc))
         (name (and (string-match "^\\(.+?\\)\\.?$" name)
                    (match-string 1 name)))
         (doc (if (or (null doc) (string= doc "")) ""
                (format "\n%s" doc)))
         (toggle (intern (format "vimp-%s-state" state)))
         (mode (intern (format "%s-minor-mode" toggle)))
         (keymap (intern (format "%s-map" toggle)))
         (local (intern (format "%s-local-minor-mode" toggle)))
         (local-keymap (intern (format "%s-local-map" toggle)))
         (tag (intern (format "%s-tag" toggle)))
         (message (intern (format "%s-message" toggle)))
         (cursor (intern (format "%s-cursor" toggle)))
         (entry-hook (intern (format "%s-entry-hook" toggle)))
         (exit-hook (intern (format "%s-exit-hook" toggle)))
         (modes (intern (format "%s-modes" toggle)))
         (predicate (intern (format "%s-p" toggle)))
         arg cursor-value enable entry-hook-value exit-hook-value
         input-method key message-value suppress-keymap tag-value)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :tag)
        (setq tag-value arg))
       ((eq key :message)
        (setq message-value arg))
       ((eq key :cursor)
        (setq cursor-value arg))
       ((eq key :entry-hook)
        (setq entry-hook-value arg)
        (unless (listp entry-hook-value)
          (setq entry-hook-value (list entry-hook-value))))
       ((eq key :exit-hook)
        (setq exit-hook-value arg)
        (unless (listp exit-hook-value)
          (setq exit-hook-value (list entry-hook-value))))
       ((eq key :enable)
        (setq enable arg))
       ((eq key :input-method)
        (setq input-method arg))
       ((eq key :suppress-keymap)
        (setq suppress-keymap arg))))

    ;; macro expansion
    `(progn
       ;; Save the state's properties in `vimp-state-properties' for
       ;; runtime lookup. Among other things, this information is used
       ;; to determine what keymaps should be activated by the state
       ;; (and, when processing :enable, what keymaps are activated by
       ;; other states). We cannot know this at compile time because
       ;; it depends on the current buffer and its active keymaps
       ;; (to which we may have assigned state bindings), as well as
       ;; states whose definitions may not have been processed yet.
       (vimp-put-property
        'vimp-state-properties ',state
        :name ',name
        :toggle ',toggle
        :mode (defvar ,mode nil
                ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." name toggle))
        :keymap (defvar ,keymap (make-sparse-keymap)
                  ,(format "Keymap for %s." name))
        :local (defvar ,local nil
                 ,(format "Non-nil if %s is enabled.
Use the command `%s' to change this variable." name toggle))
        :local-keymap (defvar ,local-keymap nil
                        ,(format "Buffer-local keymap for %s." name))
        :tag (defvar ,tag ,tag-value
               ,(format "Mode line tag for %s." name))
        :message (defvar ,message ,message-value
                   ,(format "Echo area message for %s." name))
        :cursor (defvar ,cursor ',cursor-value
                  ,(format "Cursor for %s.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above." name))
        :entry-hook (defvar ,entry-hook nil
                      ,(format "Hooks to run when entering %s." name))
        :exit-hook (defvar ,exit-hook nil
                     ,(format "Hooks to run when exiting %s." name))
        :modes (defvar ,modes nil
                 ,(format "Modes that should come up in %s." name))
        :input-method ',input-method
        :predicate ',predicate
        :enable ',enable)

       ,@(when suppress-keymap
           `((set-keymap-parent ,keymap vimp-suppress-map)))

       (dolist (func ',entry-hook-value)
         (add-hook ',entry-hook func))

       (dolist (func ',exit-hook-value)
         (add-hook ',exit-hook func))

       (defun ,predicate (&optional state)
         ,(format "Whether the current state is %s.
\(That is, whether `vimp-state' is `%s'.)" name state)
         (and vimp-local-mode
              (eq (or state vimp-state) ',state)))

       ;; define state function
       (defun ,toggle (&optional arg)
         ,(format "Enable %s. Disable with negative ARG.
If ARG is nil, don't display a message in the echo area.%s" name doc)
         (interactive)
         (cond
          ((and (numberp arg) (< arg 1))
           (setq vimp-previous-state vimp-state
                 vimp-state nil)
           (let ((vimp-state ',state))
             (run-hooks ',exit-hook)
             (setq vimp-state nil)
             (vimp-normalize-keymaps)
             ,@body))
          (t
           (unless vimp-local-mode
             (vimp-local-mode 1))
           (let ((vimp-next-state ',state)
                 input-method-activate-hook
                 input-method-deactivate-hook)
             (vimp-change-state nil)
             (setq vimp-state ',state)
             (vimp-add-to-alist 'vimp-previous-state-alist
                                ',state vimp-previous-state)
             (let ((vimp-state ',state))
               (vimp-normalize-keymaps)
               (if ',input-method
                   (activate-input-method vimp-input-method)
                 ;; BUG #475: Deactivate the current input method only
                 ;; if there is a function to deactivate it, otherwise
                 ;; an error would be raised. This strange situation
                 ;; should not arise in general and there should
                 ;; probably be a better way to handle this situation.
                 (if deactivate-current-input-method-function
                     (deactivate-input-method)))
               (unless vimp-no-display
                 (vimp-refresh-cursor ',state)
                 (vimp-refresh-mode-line ',state)
                 (when (vimp-called-interactively-p)
                   (redisplay)))
               ,@body
               (run-hooks ',entry-hook)
               (when (and vimp-echo-state
                          arg (not vimp-no-display) ,message)
                 (if (functionp ,message)
                     (funcall ,message)
                   (vimp-echo "%s" ,message))))))))

       (vimp-set-command-property ',toggle :keep-visual t)
       (vimp-set-command-property ',toggle :suppress-operator t)

       (vimp-define-keymap ,keymap nil
         :mode ,mode
         :func nil)

       (vimp-define-keymap ,local-keymap nil
         :mode ,local
         :local t
         :func nil)

       ',state)))

(provide 'vimp-core)

;;; vimp-core.el ends here
