;;; init-denote.el --- denote                        -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp, lisp

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

(require 'denote)
(require 'consult-denote)
(setq denote-directory "~/Documents/Org/denote")
;; consult-denote 支持使用 fd 查找文件名称
;; consult-denote 支持使用 riggrep 查找文件内容
(setq consult-denote-grep-command #'consult-ripgrep)
(setq consult-denote-find-command #'consult-fd)
(setq denote-date-prompt-use-org-read-date t)

;; set consult-notes-3 设置须要查询的 denote 文件夹
(add-list-to-list  'consult-notes-file-dir-sources
                   `(("Denote Notes"  ?d ,denote-directory)
                     ("Denote Notes"  ?d ,(concat denote-directory "/journal"))
                     ("Denote Notes"  ?d ,(concat denote-directory "/work"))))

(defun create-denote--in-journal-subdir ()
  (interactive)
  (let ((denote-directory (concat denote-directory "/journal")))
    (denote-org-capture)))

(push '("d" "New note (with Denote)" plain
        (file denote-last-path)
        #'create-denote--in-journal-subdir
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

;; (push '("j" "Journal" entry
;;         (file denote-journal-extras-path-to-new-or-existing-entry)
;;         "* %U %?\n%i\n%a"
;;         :kill-buffer t
;;         :empty-lines 1) org-capture-templates)

(defun my/denote-create-note-in-any-directory ()
  "Create new Denote note in any directory.
Prompt for the directory using minibuffer completion."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory (read-directory-name "New note in: " (concat denote-directory "/")  nil :must-match)))
    (call-interactively 'denote)))
;; 当 consult-denote 开启后, consult-buffer 有更多 section 片段,
;; denote buffer, denote subdir, denote silo buffer
(consult-denote-mode 1)
(provide 'init-denote)
;;; init-denote.el ends here
