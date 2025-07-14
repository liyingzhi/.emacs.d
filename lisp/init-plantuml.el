;;; init-plantuml.el --- lisp                        -*- lexical-binding: t; -*-

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

;;; Commentary:

;;

;;; Code:

;; plantuml-mode
(require 'plantuml-mode)
(setq plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
      org-plantuml-jar-path plantuml-jar-path
      plantuml-default-exec-mode 'jar
      plantuml-svg-background "white")

;; plantuml-emacs
(require 'plantuml)
(setq plantuml-output-type "svg"
      plantuml-relative-path "./images/"
      plantuml-theme "plain"
      plantuml-font "somefont"
      plantuml-add-index-number t
      plantuml-log-command t
      plantuml-mindmap-contains-org-content t
      plantuml-org-headline-bold t)

;; Enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; Enable plantuml-mode for org files src block
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(provide 'init-plantuml)
;;; init-plantuml.el ends here
