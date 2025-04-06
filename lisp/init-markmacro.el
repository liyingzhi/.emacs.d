;;; init-markmacro.el --- mark macro                 -*- lexical-binding: t; -*-

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

(require 'markmacro)

(one-key-create-menu
 "MARK-MACRO"
 '(
   (("w" . "Mark word") . markmacro-mark-words) ;标记当前符号的单词或者选中区域的单词
   (("s" . "Mark symbol") . markmacro-mark-symbols) ;标记当前符号
   (("l" . "Mark line") . markmacro-mark-lines)     ;标记非空行
   (("c" . "Mark char") . markmacro-mark-chars)     ;标记当前字符
   (("p" . "Mark parameters") . markmacro-mark-parameters) ;标记参数
   (("i" . "Mark imenu") . markmacro-mark-imenus) ;标记函数或变量

   (("G" . "Secondary region set") . markmacro-secondary-region-set) ;设置二级选中区域
   (("j" . "Secondary region mark cursor") . markmacro-secondary-region-mark-cursors) ;标记二级选中区域内的光标对象

   (("a" . "Apply all") . markmacro-apply-all)
   (("f" . "Apply all except first") . markmacro-apply-all-except-first)

   (("r" . "Rect set") . markmacro-rect-set)
   (("C" . "Rect mark column") . markmacro-rect-mark-columns)
   (("S" . "Rect mark symbol") . markmacro-rect-mark-symbols)
   (("D" . "Rect delete") . markmacro-rect-delete)
   (("R" . "Rect replace") . markmacro-rect-replace)
   (("I" . "Rect insert") . markmacro-rect-insert)
   )
 t)

(provide 'init-markmacro)
;;; init-markmacro.el ends here
