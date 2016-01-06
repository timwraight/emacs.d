;;; mu4e.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Keywords: email
;; Version: 0.0

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(require 'mu4e-meta)     ;; autogenerated file with metadata (version etc.)
(require 'mu4e-headers)  ;; headers view
(require 'mu4e-view)     ;; message view
(require 'mu4e-main)     ;; main screen
(require 'mu4e-compose)  ;; message composition / sending
(require 'mu4e-proc)     ;; communication with backend
(require 'mu4e-utils)    ;; utility functions
(require 'mu4e-context)  ;; support for contexts
(require 'mu4e-speedbar) ;; support for speedbar


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; register our handler functions; these connect server messages to functions
;; to handle them.
;;
;;
;; these are all defined in mu4e-headers
(setq mu4e-update-func  'mu4e~headers-update-handler)
(setq mu4e-header-func  'mu4e~headers-header-handler)
(setq mu4e-found-func   'mu4e~headers-found-handler)
(setq mu4e-view-func    'mu4e~headers-view-handler)
(setq mu4e-remove-func  'mu4e~headers-remove-handler)
(setq mu4e-erase-func   'mu4e~headers-clear)

;; these ones are defined in mu4e-utils
(setq mu4e-info-func    'mu4e-info-handler)
(setq mu4e-error-func   'mu4e-error-handler)
;; note: mu4e-utils also dynamically (temporarily)
;; registers mu4e-pong func

;; this one is defined in mu4e-compose
(setq mu4e-compose-func 'mu4e~compose-handler)
;; note: mu4e-compose.el dynamically registers mu4e-sent-func
;; we don't do that here, because it's only a local (temporary)
;; handler

;; this one is defined in mu4e-view
(setq mu4e-temp-func 'mu4e~view-temp-handler)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun mu4e (&optional background)
  "If mu4e is not running yet, start it. Then, show the main
window, unless BACKGROUND (prefix-argument) is non-nil."
  (interactive "P")
  ;; start mu4e, then show the main view
  (mu4e~start (unless background 'mu4e~main-view)))

(defun mu4e-quit()
  "Quit the mu4e session."
  (interactive)
  (if mu4e-confirm-quit
	  (when (y-or-n-p (mu4e-format "Are you sure you want to quit?"))
		(mu4e~stop))
	(mu4e~stop)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'mu4e)
