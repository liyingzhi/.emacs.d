;;; lib-gt.el --- gt lib                             -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

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

;;

;;; Code:

(require 'gt)

(defun gt--translate (dict)
  "Translate using DICT from the preset tranlators."
  (gt-start (alist-get dict gt-preset-translators)))

(defun gt-translate-prompt ()
  "Translate with prompt using the multiple dictionaries."
  (interactive)
  (gt--translate 'multi-dict))

(defun gt-translate-prompt-with-simple-dict ()
  "Translate with prompt using the simple dictionaries without any selection."
  (interactive)
  (gt--translate 'simple-dict))

(defun gt-use-korean-gt ()
  "Handle the texts with the utilities."
  (interactive)
  (gt--translate 'korean-gt))

(defun gt-use-text-utility ()
  "Handle the texts with the utilities."
  (interactive)
  (gt--translate 'Text-Utility))

;; from https://github.com/agzam/google-translate/blob/translate-popup/google-translate-posframe.el
(defun gt--at-paragraph-boundary-p ()
  "Return non-nil if point is within 5 characters of a paragraph boundary."
  (save-excursion
    (let ((orig-point (point))
          (threshold 5))
      (or
       (progn
         (forward-paragraph -1)
         (skip-chars-forward " \t\n")
         (<= (abs (- (point) orig-point)) threshold))
       (progn
         (goto-char orig-point)
         (forward-paragraph 1)
         (skip-chars-backward " \t\n")
         (<= (abs (- (point) orig-point)) threshold))))))

(defun gt--at-line-beginning-p ()
  "Return t if point is at the beginning of a sentence on the current line."
  (and (looking-at "[A-Z0-9]")
       (looking-back "[.?!]\\s-+" (line-beginning-position))))

(defun gt--taker-paragraph-p ()
  "Return non-nil if the current context suggests paragraph-based text capture."
  (or (cl-find major-mode '(Info-mode help-mode helpful-mode devdocs-mode))
      (gt--at-paragraph-boundary-p)))

;;; gt with AI
(defvar my-ai-oneshot-models nil
  "List of AI models available for one-shot translation.")

(defcustom my-ai-oneshot-prompts
  (list "优化、润色文本" "逐句解释代码用法" "请认真分析代码，修复代码存在的错误，并给出建议")
  "List of preset prompts for AI one-shot translation tasks."
  :type '(repeat string)
  :group 'lib-gt)

(defvar my-ai-oneshot-last-model nil
  "Last used AI model for one-shot translation.")

(defvar my-ai-oneshot-history nil
  "History of prompts used in one-shot translation.")

(defun my/gt-ai-oneshot ()
  "One-shot AI translation/prompting.
Select prompt from history or presets. Use C-. and C-, to switch AI model.
The selected text is sent to the AI model with the chosen prompt."
  (interactive)
  (let ((prompt nil)
        (model (or my-ai-oneshot-last-model
                   (setq my-ai-oneshot-last-model (or (car my-ai-oneshot-models) gt-chatgpt-model)))))
    (cl-flet ((get-cands ()
                (cl-delete-duplicates
                 (append my-ai-oneshot-history my-ai-oneshot-prompts) :from-end t :test #'equal))
              (change-model (&optional prev)
                (let* ((pos (or (cl-position my-ai-oneshot-last-model my-ai-oneshot-models :test #'equal) -1))
                       (next (if prev (max 0 (1- pos)) (min (1- (length my-ai-oneshot-models)) (1+ pos)))))
                  (setq my-ai-oneshot-last-model (nth next my-ai-oneshot-models))
                  (setq model my-ai-oneshot-last-model)
                  (overlay-put (car (overlays-at 1)) 'after-string my-ai-oneshot-last-model))))
      (setq prompt
            (minibuffer-with-setup-hook
                (lambda ()
                  (local-set-key (kbd "C-,") (lambda () (interactive) (change-model)))
                  (local-set-key (kbd "C-.") (lambda () (interactive) (change-model t)))
                  ;; (local-set-key (kbd "C-c C-k") #'my-ai-oneshot-delete-current)
                  (overlay-put (make-overlay 1 9) 'after-string model)
                  (use-local-map (make-composed-keymap nil (current-local-map))))
              (completing-read "Prompt (): " (get-cands) nil nil nil 'my-ai-oneshot-history)))

      (gt-start (gt-translator
                 :taker (gt-taker
                         :text 'point :pick nil
                         :prompt (lambda (translator)
                                   (let ((text (car (oref translator text))))
                                     (oset translator text
                                           (list (if (string-blank-p text)
                                                     prompt
                                                   (let ((str (if (string-blank-p prompt) text
                                                                (format "%s\n\n内容如下:\n\n%s\n" prompt text))))
                                                     (if current-prefix-arg (read-string "Ensure: " str) str))))))
                                   (message "Processing...")))
                 :engines (gt-chatgpt-engine
                           :cache nil
                           :stream t
                           :model model
                           :timeout 300
                           :prompt #'identity)
                 :render (gt-buffer-render
                          :name (format "*ai-oneshot-%s*" model)
                          :mode 'markdown-mode
                          :init (lambda () (markdown-toggle-markup-hiding 1))
                          :dislike-header t
                          :dislike-source t
                          :window-config '((display-buffer-below-selected))))))))


(provide 'lib-gt)
;;; lib-gt.el ends here
