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

(require 'go-translate)
(setq gt-langs '(en zh))

(if user/aider-deepseek-api
    (let* ((info (lizqwer/api-key-from-auth-source "deepseek.com")))
      (setq  gt-chatgpt-key info)
      (setq gt-chatgpt-host "https://api.deepseek.com")
      (setq gt-chatgpt-path "/chat/completions")
      (setq gt-chatgpt-model "deepseek-chat")
      (defvar my-ai-oneshot-models
        (list "deepseek-chat" "deepseek-reasoner")))
  (let* ((info (lizqwer/api-key-from-auth-source "dashscope.aliyuncs.com")))
    (setq  gt-chatgpt-key info)
    (setq gt-chatgpt-host "https://dashscope.aliyuncs.com")
    (setq gt-chatgpt-path "/compatible-mode/v1/chat/completions")
    (setq gt-chatgpt-model user/gt-chatgpt-model)

    (defvar my-ai-oneshot-models (mapcar #'symbol-name user/qwen-models))))

(defvar my-ai-oneshot-prompts
  (list "优化、润色文本" "逐句解释代码用法" "请认真分析代码，修复代码存在的错误，并给出建议"))

(defvar my-ai-oneshot-last-model nil)

(defvar my-ai-oneshot-history nil)

(defun my/gt-ai-oneshot ()
  "Use C-. C-, to switch model."
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
                           ;; :stream t
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

(setq gt-default-translator
      (gt-translator
       ;; :taker   (gt-taker :text 'buffer :pick 'paragraph) ; 配置拾取器
       :engines (list (gt-bing-engine)   ; 指定多引擎 bing
                      (gt-chatgpt-engine ; 指定多引擎 chatgpt
                       :prompt "Translate the text to {{lang}} and return result:\n\n{{text}}"))
       :render  (gt-buffer-render))) ; 配置渲染器


(provide 'init-go-translate)
;;; init-go-translate.el ends here
