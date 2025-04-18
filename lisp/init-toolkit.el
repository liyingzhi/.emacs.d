;;; init-toolkit.el --- toolkit                      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

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

;; referenced by manateelazycat/lazycat-emacs

;;; Commentary:

;;

;;; Code:

(require 'thing-edit)
(require 'move-text)
(require 'delete-block)
(require 'duplicate-line)
(require 'open-newline)
(require 'init-thing-edit)
(require 'init-markmacro)

;;;###autoload
(defun +lizqwer/toggle-move-style ()
  (interactive)
  (if (null user/move-sytle-motion)
      (setq user/move-sytle-motion t)
    (setq user/move-sytle-motion nil)))

;;some tool function
;;;###autoload
(defun scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(one-key-create-menu
 "TOOL-KIT"
 '(
   ;; open-newline.
   (("b" . "Open newline below") . open-newline-below)
   (("a" . "Open newline above") . open-newline-above)
   ;; move-text.
   (("d" . "Move text down") . move-text-down)
   (("u" . "Move text up") . move-text-up)
   ;; duplicate-line.
   (("n" . "Duplicate below") . duplicate-line-or-region-below)
   (("p" . "Duplicate above") . duplicate-line-or-region-above)
   (("N" . "Duplicate below comment") . duplicate-line-below-comment)
   (("P" . "Duplicate above comment") . duplicate-line-above-comment)
   (("O" . "comment or uncomment") . comment-or-uncomment-region)
   )
 t)

(defun my/one-key-menu-auto-popup-advice (fn)
  (let ((one-key-popup-window t))
    (funcall fn)))

(advice-add 'one-key-menu-thing-edit :around #'my/one-key-menu-auto-popup-advice)
(advice-add 'one-key-menu-mark-macro :around #'my/one-key-menu-auto-popup-advice)
(advice-add 'one-key-menu-tool-kit :around #'my/one-key-menu-auto-popup-advice)

(provide 'init-toolkit)
;;; init-toolkit.el ends here
