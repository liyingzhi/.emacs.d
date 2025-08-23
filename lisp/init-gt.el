;;; init-go-translate.el --- init go translate package  -*- lexical-binding: t; -*-

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

(require 'gt)
(setq gt-langs '(en zh))

(setq gt-default-translator
      (gt-translator
       ;; :taker   (gt-taker :text 'buffer :pick 'paragraph)       ; 配置拾取器
       :engines (list (gt-bing-engine)) ; 指定多引擎
       :render  (gt-buffer-render)))                            ; 配置渲染器

(defun pop-to-gt-result-buffer-if-exists ()
  "If the buffer name is \"gt-buffer-render-buffer-name\", then call \"pop-to-buffer\" to switch it."
  (interactive)
  (let* ((buf (get-buffer gt-buffer-render-buffer-name))
         (wins (and buf (get-buffer-window-list buf nil 0))))
    (if wins
        ;; 有一个或多个窗口显示该 buffer，则切换焦点到第一个窗口
        (pop-to-buffer buf)
      (message "No *gt‑result* buffer"))))

(add-hook #'gt-buffer-render-output-hook  #'(lambda ()
                                              (pop-to-gt-result-buffer-if-exists)))

(provide 'init-gt)
;;; init-go-translate.el ends here
