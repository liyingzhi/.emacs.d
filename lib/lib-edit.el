;;; lib-edit.el --- edit functions                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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

;;

;;; Code:

(defun my/copy-no-properties ()
  "Copy the mark place with no properties."
  (interactive)
  (when (use-region-p)
    (kill-new
     (buffer-substring-no-properties (region-beginning) (region-end)))))

;;; insert
;;;###autoload
(defun my/insert-number-lines (start-at end-at step format)
  "Insert numbered lines in rectangle selection.
When called interactively, prompt for START-AT, END-AT, STEP, and FORMAT.
START-AT is the number to start counting from.
END-AT is the number to end counting at.
STEP is the increment between numbers.
FORMAT is the format string for each number (e.g., \"%d \" or \"%02d.\")."
  (interactive
   (list (read-number "Number to count from: " 1)
         (read-number "Number to count end: " 5)
         (read-number "step: " 1)
         (read-string "Format string: "
                      "%d ")))
  (save-excursion
    (dolist (i (number-sequence start-at end-at step))
      (insert (format format i))
      (newline-and-indent))))

;;;###autoload
(defun my/new-next-item-function-byScene ()
  "Insert a new item based on the current context and scene.
In `org-mode', insert a new TODO heading.
In comments, insert a new commented line.
Otherwise, jump out of the current pair and insert a newline."
  (interactive)
  (cond
   ((derived-mode-p 'org-mode)
    (call-interactively #'org-meta-return-auto))
   ((nth 4 (syntax-ppss)) ; Inside a comment
    (comment-indent-new-line))
   (t
    (fingertip-jump-out-pair-and-newline))))

;;; Navigation

;;;###autoload
(defun goto-percent (percent)
  "Goto PERCENT of buffer.
Move point to the position that is PERCENT percent through the buffer.
For example, (goto-percent 50) moves to the middle of the buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun scroll-up-1/3 ()
  "Scroll up one third of the window height."
  (interactive)
  (scroll-up (/ (window-body-height) 3)))

(defun scroll-down-1/3 ()
  "Scroll down one third of the window height."
  (interactive)
  (scroll-down (/ (window-body-height) 3)))

(defun scroll-other-window-up-1/3 ()
  "Scroll other window up one third of the window height."
  (interactive)
  (scroll-other-window (/ (window-body-height) 3)))

(defun scroll-other-window-down-1/3 ()
  "Scroll other window down one third of the window height."
  (interactive)
  (scroll-other-window-down (/ (window-body-height) 3)))

(defun previous-buffer-dedicated-window ()
  "Switch to the previous buffer and make the window dedicated to it."
  (interactive)
  (toggle-window-dedicated (get-buffer-window (current-buffer)) nil nil)
  (previous-buffer)
  (toggle-window-dedicated (get-buffer-window (current-buffer)) t nil))

(defun next-buffer-dedicated-window ()
  "Switch to the next buffer and make the window dedicated to it."
  (interactive)
  (toggle-window-dedicated (get-buffer-window (current-buffer)) nil nil)
  (next-buffer)
  (toggle-window-dedicated (get-buffer-window (current-buffer)) t nil))

;;; Other
;;;###autoload
(defun toggle-sub-word-or-super-word ()
  "Toggle between subword mode and superword mode."
  (interactive)
  (if (bound-and-true-p subword-mode)
      (progn
        (superword-mode)
        (message "开启 super-word-mode"))
    (subword-mode)
    (message "开启 sub-word-mode")))

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun get-file-path ()
  "Get file path."
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun +lizqwer/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (car (last (file-name-split (get-file-path))))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun +lizqwer/copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (get-file-path)))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))

(provide 'lib-edit)
;;; lib-edit.el ends here
