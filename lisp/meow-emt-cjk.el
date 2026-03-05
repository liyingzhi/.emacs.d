;;; meow-emt-cjk.el --- emt for meow                 -*- lexical-binding: t; -*-

;; Copyright (C) 2026  fly_lilee

;; Author: fly_lilee <liyl2015@mail.ustc.edu.cn>
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

(wait-packages!
 '((emt :host github :repo "LuciusChen/emt" :files ("*.el" "module/*" "module"))
   (meow-cjk :host github :repo "LuciusChen/meow-cjk")))

;;; CJK tokenizer
;; Emacs Windows Tokenizer with ICU.
;; https://github.com/Master-Hash/ewt-rs
;; Download icu version libwet.so dynamic module init var/emt/ directory
;; https://github.com/roife/emt
;; Install emt.el first, put the module dynamic lib into `emt-lib-path'
(setopt emt-lib-path (expand-file-name "var/emt/libewt.so"
                                       user-emacs-directory))
;; (add-hook 'after-init-hook #'emt-mode)

(with-hook meow-mode
  (meow-cjk-mode))

(provide 'meow-emt-cjk)
;;; meow-emt-cjk.el ends here
