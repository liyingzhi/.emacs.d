;;; init-snippet.el --- init snippet package         -*- lexical-binding: t; -*-

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

(with-eval-after-load 'yasnippet
 (setopt yas-snippet-dirs
         (list
          (expand-file-name "config/yasnippet/snippets/"
                            user-emacs-directory))
         yas-buffer-local-condition yas-not-string-or-comment-condition)

 (advice-add #'yas-maybe-expand-abbrev-key-filter
             :around
             (lambda (orig-fn &rest args)
               (when (meow-insert-mode-p)
                 (apply orig-fn args)))))
(add-hook 'after-init-hook #'yas-global-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (add-to-list 'completion-at-point-functions #'yasnippet-capf)))

(provide 'init-snippet)
;;; init-snippet.el ends here
