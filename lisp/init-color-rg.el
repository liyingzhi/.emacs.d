;;; init-color-rg.el --- init color rg package       -*- lexical-binding: t; -*-

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


(with-eval-after-load 'color-rg
  (setq color-rg-show-lines-before-match 0)
  (setq color-rg-show-lines-after-match 0)
  (setq color-rg-search-no-ignore-file nil)
  (define-key isearch-mode-map (kbd "C-c M-r") 'isearch-toggle-color-rg))


(provide 'init-color-rg)
;;; init-color-rg.el ends here
