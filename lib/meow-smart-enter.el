;;; meow-smart-enter.el --- meow smart enter         -*- lexical-binding: t; -*-

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

(require 'goto-addr)
(require 'org)

(require 'meow-util)

(defun meow-smart-enter-goto-address ()
  "Goto address at point when meow normal mode."
  (interactive)
  (when (meow-normal-mode-p)
    (cond ((eq major-mode 'org-mode) (call-interactively #'org-return))
          (t (call-interactively #'goto-address-at-point)))))

(defun meow-smart-enter ()
  "Meow smart enter in normal state."
  (interactive)
  (cond ((eq major-mode 'org-mode) (call-interactively #'org-return))
        ((and (featurep 'telega-chat) (eq major-mode 'telega-chat-mode))
         (require 'telega-chat)
         (call-interactively #'telega-chatbuf-newline-or-input-send))
        (t
         (call-interactively #'newline-and-indent))))

(define-key goto-address-highlight-keymap (kbd "RET") #'meow-smart-enter-goto-address)

(with-eval-after-load 'meow
  (define-key meow-normal-state-keymap (kbd "RET") #'meow-smart-enter))

(provide 'meow-smart-enter)
;;; meow-smart-enter.el ends here
