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

(setq denote-directory "~/Documents/denote")
;; (setq denote-directory '("~/Documents/denote" "/mnt/data/shareData/"))

(setq denote-dired-directories denote-directory)
(add-to-list 'denote-prompts 'subdirectory)

(setq consult-denote-grep-command #'consult-ripgrep)
(setq consult-denote-find-command #'consult-fd)

(setq denote-date-prompt-use-org-read-date t)

(setq denote-org-store-link-to-heading 'context)

(add-list-to-list  'consult-notes-file-dir-sources
                   (let ((dirs (if (listp denote-directory)
                                   denote-directory
                                 (list denote-directory))))
                     (mapcar (lambda (dir)
                               (list (format "Denote Notes(%s)" dir) ?d dir))
                             dirs)))

(defun create-denote--in-work-subdir ()
  (interactive)
  (let ((denote-directory (concat denote-directory "/work")))
    (denote-org-capture)))

(defun create-denote--in-robot-subdir ()
  (interactive)
  (let ((denote-directory (concat denote-directory "/robot")))
    (denote-org-capture)))

(defun create-denote--in-personal-subdir ()
  (interactive)
  (let ((denote-directory (concat denote-directory "/personal")))
    (denote-org-capture)))

(push '("W" "New work note (with Denote)" plain
        (file denote-last-path)
        #'create-denote--in-work-subdir
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

(push '("r" "New robot note (with Denote)" plain
        (file denote-last-path)
        #'create-denote--in-robot-subdir
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

(push '("p" "New personal note (with Denote)" plain
        (file denote-last-path)
        #'create-denote--in-personal-subdir
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

;; (push '("j" "Journal" entry
;;         (file denote-journal-extras-path-to-new-or-existing-entry)
;;         "* %U %?\n%i\n%a"
;;         :kill-buffer t
;;         :empty-lines 1) org-capture-templates)

(defun my-denote-create-note-in-any-directory ()
  "Create new Denote note in any directory.
Prompt for the directory using minibuffer completion."
  (declare (interactive-only t))
  (interactive)
  (let ((denote-directory (read-directory-name "New note in: " nil nil :must-match)))
    (call-interactively 'denote)))


(provide 'init-denote)
;;; init-denote.el ends here
