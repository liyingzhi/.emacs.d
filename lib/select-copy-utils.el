;;; select-copy-utils.el --- slect and copy           -*- lexical-binding: t; -*-

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

(require 'transient)

(defun my/copy-current-line ()
  "Copy the current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (line-beginning-position)
                      (line-end-position)))))

(defun my/copy-from-point-to-end-of-current-line ()
  "Copy the content from point to the end of current line."
  (interactive)
  (kill-new
   (string-trim
    (buffer-substring (point)
                      (line-end-position)))))

(defun my/select-end-of-buffer-to-point ()
  "Select contents from the end of the buffer to the current point."
  (interactive)
  (push-mark (point-max) t t)
  (goto-char (point)))

(defun my/select-end-of-current-line-to-point ()
  "Select contents from the end of the current line to the current point."
  (interactive)
  (push-mark (line-end-position) t t)
  (goto-char (point)))

(defun my/copy-current-buffer-name ()
  "Copy the name of the current buffer to the `kill-ring'."
  (interactive)
  (let ((name (buffer-name)))
    (when name
      (kill-new name)
      (message "缓冲区名称 \"%s\" 已复制到 kill-ring。" name))))

(transient-define-prefix my/copy-select-utils-dispatch ()
  "Select and copy content menu."
  ["Utils"
   ("y" "Yank from point to endline" my/copy-from-point-to-end-of-current-line)
   ("Y" "Yank current line" my/copy-current-line)
   ("q" "Mark from point to endline" my/select-end-of-current-line-to-point)
   ("w" "Mark from point to endbuffer" my/select-end-of-buffer-to-point)
   ("o" "Mark row expreg expand" home-row-expreg-expand-with-letters)
   ("c" "Yank name of current buffer" my/copy-current-buffer-name)
   ("r" "visual-replace" visual-replace)])

(provide 'select-copy-utils)
;;; select-copy-utils.el ends here
