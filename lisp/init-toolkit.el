;;; init-toolkit.el --- toolkit                      -*- lexical-binding: t; -*-

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

(require 'thing-edit)
(require 'move-text)
(require 'delete-block)
(require 'duplicate-line)
(require 'open-newline)
(require 'init-thing-edit)
;; (require 'init-markmacro)
(require 'dwim-shell-commands)

;;;###autoload
(defun +lizqwer/toggle-move-style ()
  "Toggle window move style."
  (interactive)
  (if (null user/move-style-motion)
      (setq user/move-style-motion t)
    (setq user/move-style-motion nil)))

;;some tool function
;;;###autoload
(defun scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

;;;###autoload
(defun scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

(one-key-create-menu
 "TOOL-KIT"
 '(
   ;; open-newline.
   (("b" . "Open newline below") . open-newline-below)
   (("a" . "Open newline above") . open-newline-above)
   ;; move-text.
   (("d" . "Move text down") . move-text-down)
   (("u" . "Move text up") . move-text-up)
   ;; duplicate-line.
   (("n" . "Duplicate below") . duplicate-line-or-region-below)
   (("p" . "Duplicate above") . duplicate-line-or-region-above)
   (("N" . "Duplicate below comment") . duplicate-line-below-comment)
   (("P" . "Duplicate above comment") . duplicate-line-above-comment)
   (("O" . "comment or uncomment") . comment-or-uncomment-region)
   )
 t)
;;; sticker
;; Refs: https://github.com/karthink/.emacs.d/blob/master/lisp/utilities.el
(defvar sticker-list nil
  "Alist of stored stickers, used by `sticker'.
Each element is a list whose `car' is the sticker label (a string,
possibly carrying text properties such as a face).  The remaining
elements are the overlays currently displaying that label, most
recently created first.")

(defun sticker-next (&optional arg)
  "Move to the line of the next sticker.
With prefix argument ARG, repeat that many times; a negative ARG
moves to previous stickers instead.  Stickers are the overlays
created by `sticker' and are recognised via their `sticker'
overlay property."
  (interactive "p")
  (let ((func (if (< arg 0)
                  'previous-single-char-property-change
                'next-single-char-property-change)))
    (dotimes (_ (abs arg))
      (unless (< arg 0)
        (forward-line 1))
      (goto-char (funcall func (point) 'sticker))
      (forward-line 0))))

(defun sticker (&optional arg label)
  "Store or attach stickers (see `sticker-list').

With the region active, store its text as a sticker label and do
nothing else.  Otherwise act on the sticker overlay at point:
with prefix argument ARG re-label that overlay by choosing another
label, without ARG delete it (dropping the label from
`sticker-list' when no overlay still uses it).  When there is no
overlay at point, attach a new sticker to the current line, taking
the label from LABEL or, interactively, from the collection of
stored labels."
  (interactive "P")
  (cl-flet ((store-sticker (beg end)
              (or (assoc (buffer-substring beg end) sticker-list)
                  (push (list (buffer-substring beg end)) sticker-list))
              (deactivate-mark)
              (message "Sticker stored"))
            (new-sticker (l &optional o)
              (unless o
                (setq o (make-overlay (line-beginning-position)
                                      (line-end-position))))
              (overlay-put o 'evaporate t)
              (overlay-put o 'sticker l)
              (overlay-put
               o 'after-string
               (concat " " (if (get-text-property 0 'face l)
                               l
                             (propertize l 'face '(:inverse-video t)))))
              (push o (alist-get l sticker-list nil nil #'equal)))
            (read-label ()
              (let ((minibuffer-allow-text-properties t))
                (completing-read "Attach sticker: " sticker-list))))
    (if (use-region-p)
        (store-sticker (region-beginning) (region-end))
      (if-let* ((exist-ov (cdr-safe (get-char-property-and-overlay (point) 'sticker))))
          (if arg
              (new-sticker (read-label) exist-ov)
            (let* ((old-l (overlay-get exist-ov 'sticker))
                   (matches (assoc old-l sticker-list)))
              (delq exist-ov matches)
              (when (= (length matches) 1)
                (setq sticker-list (delete matches sticker-list)))
              (delete-overlay exist-ov)))
        (new-sticker (or label (read-label)))))))

(add-hook 'poi-functions #'sticker-next)

(global-set-keys '(("M-I" . sticker)))

(provide 'init-toolkit)
;;; init-toolkit.el ends here
