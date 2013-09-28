;;; inferior-apl --- inferior-apl mode -*- lexical-binding: t -*-

;; Copyright (C) 2013 Rüdiger Sonderfeld

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>
;; Created: 27 Sep 2013
;; Keywords: APL
;; URL: https://github.com/ruediger/apl-mode

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'apl-mode)

(defgroup inferior-apl nil
  "inferior-apl mode"
  :link '(url-link "https://github.com/ruediger/apl-mode")
  :prefix "inferior-apl-"
  :group 'apl)

(defcustom inferior-apl-program (executable-find "apl")
  "Program invoked by `inferior-apl'."
  :type 'file
  :group 'inferior-apl)

(defcustom inferior-apl-buffer "*APL*"
  "Name of the `inferior-apl' buffer."
  :type 'string
  :group 'inferior-apl)

(defcustom inferior-apl-prompt-read-only t
  "If non-nil then make the prompt read only.
See `comint-prompt-read-only' for details."
  :type 'boolean
  :group 'inferior-apl)

(defcustom inferior-apl-startup-file
  (locate-user-emacs-file "init_apl.apl")
  "Name of the startup file loaded by `inferior-apl'.
The content is sent to the APL process."
  :type '(choice (const :tag "None" nil) file)
  :group 'inferior-apl)

(defcustom inferior-apl-args nil
  "List of command line arguments for the APL process."
  :type '(repeat string)
  :group 'inferior-apl)

(defcustom inferior-apl-mode-hook nil
  "Hook to be run when `inferior-apl-mode' is started."
  :type 'hook
  :group 'inferior-apl)

(defcustom inferior-apl-prompt-regexp "^      "
  "Regexp to match prompt"
  :type 'regexp
  :group 'inferior-apl)

(defvar inferior-apl-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map comint-mode-map)
    (define-key map "\C-hd" 'apl-help)
    map)
  "Keymap for `inferior-apl-mode'.")

(define-derived-mode inferior-apl-mode comint-mode "Inferior APL"
  "Major mode for running an APL process inside Emacs.

Key bindings:
\\{inferior-apl-mode-map}"
  :syntax-table apl-mode-syntax-table
  :abbrev-table apl-mode-abbrev-table
  :group 'inferior-apl

  (setq-local eldoc-documentation-function #'apl-eldoc-function)

  (setq comint-prompt-regexp inferior-apl-prompt-regexp
        comint-use-prompt-regexp t)
  (setq-local comment-start apl-comment-start)
  (setq-local comment-end "")
  (setq-local comint-prompt-read-only inferior-apl-prompt-read-only))

(defun inferior-apl-is-running? ()
  "Return non-nil if `inferior-apl' is running."
  (comint-check-proc inferior-apl-buffer))
(defalias 'inferior-apl-is-running-p #'inferior-apl-is-running?)

(defun inferior-apl-startup ()
  "Start APL."
  (comint-exec inferior-apl-buffer
               "InferiorAPL"
               inferior-apl-program
               (when (file-exists-p inferior-apl-startup-file)
                 inferior-apl-startup-file)
               inferior-apl-args))

(defun inferior-apl (&optional arg)
  (interactive "P")
  (let ((buffer (get-buffer-create inferior-apl-buffer)))
    (unless arg
      (pop-to-buffer buffer))
    (unless (inferior-apl-is-running?)
      (with-current-buffer buffer
        (inferior-apl-startup)
        (inferior-apl-mode)))
    buffer))
(defalias 'run-apl #'inferior-apl)

(defun inferior-apl-eval-region (begin end)
  "Evaluate region between BEGIN and END."
  (interactive "r")
  (inferior-apl t)
  (comint-send-region inferior-apl-buffer begin end)
  (comint-send-string inferior-apl-buffer "\n"))

(defun inferior-apl-eval-buffer ()
  "Evaluate buffer"
  (interactive)
  (inferior-apl-eval-region (point-min) (point-max)))

(defun inferior-apl-eval-line (&optional arg)
  "Evaluate current line.
If ARG is a positive prefix then evaluate ARG number of lines starting with the
current one."
  (interactive "p")
  (unless arg
    (setq arg 1))
  (when (> arg 0)
    (inferior-apl-eval-region
     (line-beginning-position)
     (line-end-position arg))))

(provide 'inferior-apl)

;;; inferior-apl.el ends here
