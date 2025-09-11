;;; auto-string-inflection.el --- string inflection       -*- lexical-binding: t; -*-

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

;; This library provides functions for cycling and converting string inflections
;; and cases in various programming language modes. It includes support for
;; different naming conventions such as camelCase, snake_case, kebab-case, etc.
;; The functions automatically detect the current major mode to apply the
;; appropriate conversion style.

;;; Code:

(require 'string-inflection)
(require 'transient)

(defun my/string-inflection-cycle-auto ()
  "Cycle different string inflection styles based on the current major mode.
This function detects the current major mode and applies the corresponding
inflection cycle:
- emacs-lisp-mode:
foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar
- python-mode: foo_bar => FOO_BAR => FooBar => foo_bar
- java-mode: fooBar => FOO_BAR => FooBar => fooBar
- elixir-mode: foo_bar => FooBar => foo_bar
- other modes: foo_bar => FOO_BAR => FooBar => foo_bar (default ruby style)"
  (interactive)
  (cond
   ;; for emacs-lisp-mode
   ((eq major-mode 'emacs-lisp-mode)
    ;; "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
    (string-inflection-all-cycle))
   ;; for python
   ((eq major-mode 'python-mode)
    (string-inflection-python-style-cycle))
   ;; for java
   ((eq major-mode 'java-mode)
    (string-inflection-java-style-cycle))
   ;; for elixir
   ((eq major-mode 'elixir-mode)
    (string-inflection-elixir-style-cycle))
   (t
    ;; default
    (string-inflection-ruby-style-cycle))))

(defun my/string-case-cycle-auto ()
  "Intelligently cycle through word case formats:
- If all lowercase → convert to capitalized
- If capitalized → convert to all uppercase
- If all uppercase → convert to all lowercase
Moves cursor to word start (virtually, without affecting actual position)
before conversion."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (when bounds
                 (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (cond
     ((null word) (user-error "No word at point"))
     (t
      (save-excursion
        (goto-char (car bounds))
        (cond
         ((string-equal word (downcase word))   ; All lowercase → Capitalized
          (call-interactively #'capitalize-dwim))
         ((string-equal word (capitalize word)) ; Capitalized → All uppercase
          (call-interactively #'upcase-dwim))
         ((string-equal word (upcase word))     ; All uppercase → All lowercase
          (call-interactively #'downcase-dwim))
         (t (call-interactively #'downcase-dwim))))))))

(defun my/string-customize-convert-with-parameter (&optional args)
  "Convert string inflection or case based on provided style parameters.
This function accepts ARGS as a list of style parameters and converts the
current word or symbol according to the specified style:
- For inflection styles: foo_bar, FOO_BAR, Foo_Bar, foo-bar, FooBar, fooBar
- For case styles: foobar (lowercase), FOOBAR (uppercase), Foobar (capitalized)"
  (interactive (list (transient-args 'my/string-convert-dispatch)))
  (cond ((member "--styleCamel=foo_bar" args)
         (string-inflection-underscore))
        ((member "--styleCamel=FOO_BAR" args)
         (string-inflection-upcase))
        ((member "--styleCamel=Foo_Bar" args)
         (string-inflection-capital-underscore))
        ((member "--styleCamel=foo-bar" args)
         (string-inflection-kebab-case))
        ((member "--styleCamel=FooBar" args)
         (string-inflection-camelcase))
        ((member "--styleCamel=fooBar" args)
         (string-inflection-lower-camelcase))
        ((member "--styleCase=foobar" args)
         (call-interactively #'downcase-dwim))
        ((member "--styleCase=Foobar" args)
         (call-interactively #'capitalize-dwim))
        ((member "--styleCase=FOOBAR" args)
         (call-interactively #'upcase-dwim))))

(defun my-string--cycle-description ()
  "Return a Transient menu headline showing the currently active cycle styles.
This function displays the available string inflection and case conversion
styles for the current major mode in the Transient menu."
  (format (propertize "Cycle Style: \n [%s %s \n %s %s]"
                      'face 'transient-heading)
          (propertize "string-inflection-cycle-auto:"
                      'face 'transient-inapt-suffix)
          (cond
           ;; for emacs-lisp-mode
           ((eq major-mode 'emacs-lisp-mode)
            ;; "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
            (propertize "foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar"
                        'face 'transient-inapt-suffix))
           ;; for python
           ((eq major-mode 'python-mode)
            (propertize "foo_bar => FOO_BAR => FooBar => foo_bar"
                        'face 'transient-inapt-suffix))
           ;; for java
           ((eq major-mode 'java-mode)
            (propertize "fooBar => FOO_BAR => FooBar => fooBar"
                        'face 'transient-inapt-suffix))
           ;; for elixir
           ((eq major-mode 'elixir-mode)
            (propertize "foo_bar => FooBar => foo_bar"
                        'face 'transient-inapt-suffix))
           (t
            ;; default
            (propertize "foo_bar => FOO_BAR => FooBar => foo_bar"
                        'face 'transient-inapt-suffix)))
          (propertize "string-case-cycle-auto:"
                      'face 'transient-inapt-suffix)
          (propertize "foobar => FOOBAR => Foobar"
                      'face 'transient-inapt-suffix)))

(transient-define-prefix my/string-convert-dispatch ()
  "String conversion menu for inflection and case transformations.
This Transient menu provides various options for converting string inflections
and cases, including customizable styles and fast conversion commands."
  ["Customize Arguments & Convert"
   ("-c " "UpDownCase" "--styleCase="
    :choices ("foobar" "FOOBAR" "Foobar"))
   ("-s " "Camel style" "--styleCamel="
    :choices ("foo_bar" "FOO_BAR" "Foo_Bar" "foo-bar" "FooBar" "fooBar"))
   ("RET" "Customize Convert" my/string-customize-convert-with-parameter)]

  [:description
   my-string--cycle-description
   ("s" "string-inflection-cycle-auto" my/string-inflection-cycle-auto :transient t)
   ("c" "string-case-cycle-auto [foobar => FOOBAR => Foobar]" my/string-case-cycle-auto :transient t)]

  ["Fast Convert"
   ("C" "capitalize-dwim" capitalize-dwim)
   ("U" "upcase-dwim" upcase-dwim)
   ("D" "downcase-dwim" downcase-dwim)
   ("u" "subword-upcase" subword-upcase)
   ("d" "subword-downcase" subword-downcase)]

  [("q" "Quit"           transient-quit-one)])

(provide 'auto-string-inflection)
;;; auto-string-inflection.el ends here
