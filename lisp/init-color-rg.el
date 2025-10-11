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

(when user/hidden-outline
  (advice-add 'color-rg-open-file-and-stay :before #'my/color-rg--outline-show-all))

(defun my/color-rg--outline-show-all ()
  "Show all outline headings in the current color-rg match buffer.
This function is designed to work with color-rg search results. It:
1. Gets the match file from current color-rg buffer
2. Gets the corresponding buffer containing the matches
3. Shows all outline headings in that buffer

This is typically used when you want to expand all collapsed sections
in a color-rg search result buffer."
  (when-let* ((match-file (color-rg-get-match-file))
              (match-buffer (color-rg-get-match-buffer match-file)))
    (when match-buffer
      (with-current-buffer match-buffer
        (outline-show-all)))))

(provide 'init-color-rg)
;;; init-color-rg.el ends here
