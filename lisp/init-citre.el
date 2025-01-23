;;; init-citre.el --- init citre                     -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

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

(require 'citre-config)
(require 'citre)
(setq citre-auto-enable-citre-mode-modes '(prog-mode))
(setq citre-ctags-program "/usr/local/bin/ctags")
(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)
(setq citre-find-definition-backends '(tags global))
(setq citre-find-reference-backends '(global))

;; disable imenu integration with citre backend
(setq-default citre-enable-imenu-integration nil)

(defun my/citre-delete-tags-files ()
  (interactive)
  (when (and citre--tags-file
           (not (equal citre--tags-file 'none)))
    (let* ((path (string-remove-suffix ".tags" citre--tags-file))
           (list-path (concat path ".list"))
           (ctags-path (concat path ".ctags"))
           (tags-path (concat path ".tags"))
           (promot (format "Delete: \n%s \n%s \n%s \nDelete this project tags cache files?"
                           list-path
                           ctags-path
                           tags-path)))
      (when (yes-or-no-p promot)
        (delete-file list-path)
        (delete-file ctags-path)
        (delete-file tags-path)
        (message "Delete Finished!")))))

(add-hook #'eglot-managed-mode-hook
          #'(lambda ()
              (when citre-mode
                (setq-local xref-backend-functions
                            '(citre-xref-backend
                              t)))))

(provide 'init-citre)
;;; init-citre.el ends here
