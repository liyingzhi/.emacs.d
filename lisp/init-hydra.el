;;; init-hydra.el --- init hydra                     -*- lexical-binding: t; -*-

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

(with-eval-after-load 'posframe
  (defun hydra-set-posframe-show-params ()
    "Set hydra-posframe style."
    (setq hydra-posframe-show-params
          `( :left-fringe 8
             :right-fringe 8
             :internal-border-width 2
             :internal-border-color ,(face-background 'posframe-border nil t)
             :background-color ,(face-background 'tooltip nil t)
             :foreground-color ,(face-foreground 'tooltip nil t)
             :lines-truncate t
             :poshandler posframe-poshandler-frame-center-near-bottom)))
  (hydra-set-posframe-show-params)

  (defun start-posframe ()
    (require 'hydra)
    (setq hydra-hint-display-type
          'posframe))

  (defun stop-posframe ()
    (require 'hydra)
    (setq hydra-hint-display-type
          'lv)
    (posframe-delete-all)))

(require 'pretty-hydra)

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
     (require 'nerd-icons nil t)))

(cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                    &key face height v-adjust)
  "Add an icon in the hydra title."
  (let ((face (or face `(:inherit highlight :reverse-video t)))
        (height (or height 1.2))
        (v-adjust (or v-adjust 0.0)))
    (concat
     (when (and (icons-displayable-p) icon-type icon-name)
       (let ((f (intern (format "nerd-icons-%s" icon-type))))
         (when (fboundp f)
           (concat
            (apply f (list icon-name :face face :height height :v-adjust v-adjust))
            " "))))
     (propertize title 'face face))))

(defun pretty-hydra-define-add-exit (head-plist)
  (mapcar-if #'(lambda (head)
                 (mapcar-if-not #'(lambda (key)
                                    (append key
                                            (list :exit t)))
                                head
                                #'(lambda (v)
                                    (cl-find :exit v :test #'eq))))
             head-plist
             #'listp))

;;;###autoload
(defmacro pretty-hydra-define-e (name body head-plist)
  (declare (indent 1) (debug (form def-body)))
  `(progn
     (pretty-hydra-define ,name ,body
       ,(if (cl-getf body :all-exit)
            (pretty-hydra-define-add-exit head-plist)
          head-plist))
     ,@(when (cl-getf body :posframe)
         (let ((name (symbol-name name)))
           `((advice-add #',(intern (concat name "/body")) :before #'start-posframe)
             (advice-add #',(intern (concat name "/nil")) :after #'stop-posframe)) ))))


(pretty-hydra-define-e hydra-git
  (:title "Git" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("hunk"
   (("n" diff-hl-next-hunk "Next hunk")
    ("p" diff-hl-previous-hunk "Previous hunk")
    ("s" diff-hl-show-hunk "Show hunk"))
   "git"
   (("b" magit-blame "Blame")
    ("f" magit-find-file "Find git file")
    ("l" magit-log-buffer-file "File log")
    ("h" vc-region-history "History"))))

(transient-define-prefix git-dispatch ()
  "Git dispatch menu"
  :transient-non-suffix 'transient--do-stay
  [["Hunk"
    ("n" "Next hunk" diff-hl-next-hunk  :transient t)
    ("p" "Previous hunk" diff-hl-previous-hunk :transient t)
    ("r" "Revert hunk" diff-hl-revert-hunk :transient t)
    ("s" "Show hunk" diff-hl-show-hunk)]
   ["Magit"
    ("v" "magit status" unpackaged/magit-status)
    ("d" "magit dispatch" magit-dispatch)
    ("b" "Blame" magit-blame)
    ("f" "Find git file" magit-find-file)]
   ["Log"
    ("oh" "Region history" vc-region-history)
    ("ol" "File log" magit-log-buffer-file)]]
  [("q" "Quit" transient-quit-one)])

(pretty-hydra-define-e hydra-language
  (:title "Language" :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("dict"
   (("f" fanyi-dwim2 "Fanyi Point")
    ("F" fanyi-dwim "Fanyi Input"))
   "english"
   (("t" gt-do-translate "translate"))))




(provide 'init-hydra)
