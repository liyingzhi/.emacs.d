;;; meow-emt-cjk.el --- Meow integration with EMT for CJK text processing -*- lexical-binding: t; -*-

;; Copyright (C) 2026  fly_lilee

;; Author: fly_lilee <liyl2015@mail.ustc.edu.cn>
;; Keywords: tools, meow, cjk, emt, tokenizer

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

;; This package provides integration between Meow modal editing system and
;; EMT (Emacs Windows Tokenizer) for enhanced CJK (Chinese, Japanese, Korean)
;; text processing capabilities.
;;
;; It enables intelligent CJK tokenization and word-based navigation when
;; using Meow mode, improving editing efficiency for CJK text.

;;; Code:

(wait-packages!
 '((emt :host github :repo "LuciusChen/emt" :files ("*.el" "module/*" "module"))
   (meow-cjk :host github :repo "LuciusChen/meow-cjk")))

;;; CJK tokenizer configuration
;;
;; EMT (Emacs Windows Tokenizer) is a dynamic module that provides ICU-based
;; CJK text tokenization. This enables accurate word segmentation for Chinese,
;; Japanese, and Korean text.
;;
;; Project references:
;; - EWT-RS (Emacs Windows Tokenizer in Rust): https://github.com/Master-Hash/ewt-rs
;; - EMT (Emacs Module for Tokenization): https://github.com/roife/emt
;;
;; Installation requirements:
;; 1. Install emt.el package first
;; 2. Download the ICU version of libewt.so dynamic module
;; 3. Place the module in the `emt-lib-path' directory
;;
;; The module should be located at: var/emt/libewt.so relative to user-emacs-directory
(setopt emt-lib-path (expand-file-name "var/emt/libewt.so"
                                       user-emacs-directory))
;; (add-hook 'after-init-hook #'emt-mode)

(with-hook meow-mode
  (meow-cjk-mode))

(provide 'meow-emt-cjk)
;;; meow-emt-cjk.el ends here
