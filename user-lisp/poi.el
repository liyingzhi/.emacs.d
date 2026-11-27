;;; poi.el --- Jump to points of interest            -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Jump to next/previous Point-Of-Interest
;;
;; This package provides the commands `poi-next' and `poi-previous' to navigate
;; between points of interest in the buffer.  A point-of-interest is any
;; location with some (usually diagnostic) importance.
;;
;; `poi-select-error' lists every point of interest in the current buffer and
;; jumps to the one picked from a `completing-read' prompt.
;;
;; To define a point-of-interest, you need a function FUNC that moves point to
;; the next instance of something, and to the previous instance when called with
;; an arg of -1.
;;
;; Then you can add it as a point-of-interest source via
;;
;; (poi-register FUNC)
;;
;; For example, here we add three diagnostic sources to poi in the current
;; buffer:
;;
;; (poi-register 'flymake-goto-next-error)
;; (poi-register 'next-error)
;; (poi-register 'jinx-next)
;;
;; (`poi-register' works buffer-locally, so you will typically want to do this
;; in a hook.)
;;
;; Now you can use `poi-next' and `poi-previous' to jump to the next/previous
;; instance of all of these, in the order of increasing/decreasing cursor
;; position.

;;; Code:
(require 'simple)

(defcustom poi-functions nil
  "Additional functions to use as `next-error-function'."
  :group 'next-error
  :type 'hook)

(defvar-local poi--choice nil)

(defun poi-action ()
  "Perform the logical action at point of interest."
  (interactive)
  (and-let* ((poi--choice)
             (action-fn (get poi--choice 'poi-action)))
    (call-interactively action-fn)))

(defun poi-next (arg)
  (interactive "p")
  (let ((final-point (if (> arg 0) (point-max) (point-min)))
        (comparison (if (> arg 0) #'< #'>))
        next-point (here (point)))
    (run-hook-wrapped
     'poi-functions
     (lambda (func)
       (prog1 nil
         (save-excursion
           (condition-case nil (funcall func arg)
             (user-error nil)
             (:success
              (when (and (funcall comparison (point) (or next-point final-point))
                         (funcall comparison here (point)))
                (setq poi--choice func
                      next-point (point)))))))))
    (if next-point (goto-char next-point) (ding t))))

(defun poi-previous (arg)
  (interactive "p")
  (poi-next (- arg)))

;;;###autoload
(defun poi-register (fn &optional action-fn)
  "Add FUN to `poi-functions'."
  (lambda ()
    (if (memq fn poi-functions)
        (remove-hook 'poi-functions fn 'local)
      (add-hook 'poi-functions fn nil 'local))
    (when action-fn (put fn 'poi-action action-fn))))

;;;; Selecting a point of interest
(defvar poi--history nil
  "Minibuffer history for `poi-select-error'.")

(defun poi--errors ()
  "Return all points of interest in the current buffer.
Each element is a cons cell (POSITION . SOURCE), where SOURCE is
the function from `poi-functions' that found POSITION."
  (save-excursion
    (goto-char (point-min))
    (let ((inhibit-message t)
          (ring-bell-function #'ignore)
          errors pos)
      (while (setq pos (poi-next 1))
        (push (cons pos poi--choice) errors))
      (nreverse errors))))

(defun poi--line-text (pos)
  "Return the trimmed buffer text of the line at POS."
  (save-excursion
    (goto-char pos)
    (string-trim (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))

(defun poi--error-candidate (pos source)
  "Return a completion candidate string describing the POI at POS.
SOURCE is the function from `poi-functions' that found POS.  The
description is POS's `help-echo' text when it has some, and the
text of POS's line otherwise."
  (let* ((help-echo (get-char-property-and-overlay pos 'help-echo))
         (text (cond ((stringp (car help-echo))
                      (car help-echo))
                     ((functionp (car help-echo))
                      (ignore-errors
                        (funcall (car help-echo) (selected-window)
                                 (cdr help-echo) pos)))))
         (desc (subst-char-in-string ?\n ?\s (or text (poi--line-text pos))))
         (column (save-excursion (goto-char pos) (current-column))))
    (format "%-24s  %5d:%-4d %s"
            (symbol-name source)
            (line-number-at-pos pos t) (1+ column)
            (if (> (length desc) 40) (concat (substring desc 0 40) "...") desc))))

;;;###autoload
(defun poi-select-error ()
  "Jump to one of the points of interest in the current buffer."
  (interactive)
  (if-let* ((errors (poi--errors)))
      (let* ((candidates (mapcar
                          (lambda (err)
                            (cons (poi--error-candidate (car err) (cdr err)) err))
                          errors))
             (default (car (seq-find (lambda (cell) (<= (point) (car (cdr cell))))
                                     candidates)))
             (choice (completing-read
                      (format "Points of interest (%d): " (length errors))
                      candidates nil t nil 'poi--history default))
             (picked (cdr (assoc choice candidates))))
        (goto-char (car picked))
        (setq poi--choice (cdr picked))
        (next-error-found (current-buffer)))
    (user-error "No points of interest in buffer")))

;;;; Integration with `next-error'
(defun poi-delegate (orig-fn &optional arg reset)
  "If a `next-error' source is visible, run `next-error'.

Otherwise jump to the closest point of interest."
  (if poi-functions
      (if-let* ((buf (ignore-errors
                       (next-error-find-buffer
                        t nil (lambda () (not (memq next-error-function
                                                    '(org-occur-next-match
                                                      vterm-next-error-function)))))))
                ((get-buffer-window buf)))
          (funcall orig-fn arg reset)
        (setq arg (or arg 1))
        (poi-next arg))
    (funcall orig-fn arg reset)))

(provide 'poi)
;;; poi.el ends here
