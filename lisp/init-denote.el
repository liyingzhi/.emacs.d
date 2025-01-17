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
(setq consult-denote-grep-command #'consult-ripgrep)
(setq consult-denote-find-command #'consult-fd)

;; (push '("b" "笔记" entry (file+headline "~/Org/note.org" "笔记") "* %^{标题} %t\n  %?\n") org-capture-templates)
(push '("d" "New note (with Denote)" plain
        (file denote-last-path)
        #'denote-org-capture
        :no-save t
        :immediate-finish nil
        :kill-buffer t
        :jump-to-captured t) org-capture-templates)

(consult-denote-mode 1)
(provide 'init-denote)
;;; init-denote.el ends here
