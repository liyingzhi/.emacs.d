;;; init-gt.el --- emacs lisp                        -*- lexical-binding: t; -*-

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


(require 'gt)
(setq gt-langs '(en zh)
      gt-buffer-render-follow-p t
      gt-buffer-render-window-config
      '((display-buffer-reuse-window display-buffer-in-direction)
        (direction . right)
        (window-width . 0.35)))

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

(setq gt-preset-translators
      `((default . ,(gt-translator
                     :taker   (list (gt-taker :pick nil :if 'selection)
                                    (gt-taker :text 'paragraph :if '(Info-mode help-mode helpful-mode devdocs-mode telega-webpage-mode elfeed-show-mode))
                                    (gt-taker :text 'buffer :pick 'fresh-word
                                              :if (lambda (translatror)
                                                    (and (not (derived-mode-p 'fanyi-mode)) buffer-read-only)))
                                    (gt-taker :text 'word))
                     :engines (list (gt-google-engine :if '(and not-word))
                                    ;; (gt-bing-engine :if '(and not-word))
                                    (gt-youdao-dict-engine :if '(word))
                                    (gt-youdao-suggest-engine :if '(and word src:en))
                                    ;; (gt-chatgpt-engine                     ; 指定多引擎 chatgpt
                                    ;;  :prompt (concat "You are a translating assistant. Respond concisely."
                                    ;;                  " Generate ONLY the translated result text,"
                                    ;;                  " without any explanation or sentence incomplete reminder."
                                    ;;                  "\n\nTranslate the text to {{lang}} and return result:\n\n{{text}}")
                                    ;;  :stream t
                                    ;;  :if '(and not-word))
                                    )
                     :render (list (gt-overlay-render :if '(Info-mode helpful-mode devdocs-mode telega-webpage-mode elfeed-show-mode))
                                   (gt-insert-render :if '(telega-chat-mode) :type 'replace)
                                   (gt-buffer-render))))
        (multi-dict . ,(gt-translator :taker (gt-taker :prompt t)
                                      :engines (list (gt-bing-engine)
                                                     (gt-youdao-dict-engine)
                                                     (gt-youdao-suggest-engine :if 'word)
                                                     (gt-google-engine))
                                      :render (gt-buffer-render)))
        (Text-Utility . ,(gt-text-utility :taker (gt-taker :pick nil)
                                          :render (gt-buffer-render)))
        (korean-gt . ,(gt-translator
                       :taker   (list (gt-taker :langs '(ko zh) :pick nil :if 'selection)
                                      (gt-taker :langs '(ko zh) :text 'word))
                       :engines (list (gt-bing-engine)
                                      (gt-google-engine)
                                      (gt-chatgpt-engine                     ; 指定多引擎 chatgpt
                                       :prompt (concat "You are a translating assistant. Respond concisely."
                                                       " Generate ONLY the translated result text,"
                                                       " without any explanation or sentence incomplete reminder."
                                                       "\n\nTranslate the text to {{lang}} and return result:\n\n{{text}}")
                                       :stream t
                                       :if '(and not-word)))
                       :render  (gt-buffer-render)))
        (after-source-insert . ,(gt-translator
                                 :taker (gt-taker :text 'buffer :pick 'paragraph)
                                 :engines (gt-google-engine)
                                 :render (gt-insert-render :type 'after)))
        (replace-source-chat-insert . ,(gt-translator
                                        :taker (gt-taker :text 'paragraph :pick nil)
                                        :engines (gt-google-engine)
                                        :render (gt-insert-render :type 'replace)))
        (only-translate-rare-insert . ,(gt-translator
                                        :taker (gt-taker :text 'paragraph
                                                         :pick 'word
                                                         :pick-pred (lambda (w) (length> w 6)))
                                        :engines (gt-google-engine)
                                        :render (gt-insert-render :type 'after
                                                                  :rfmt " (%s)"
                                                                  :rface '(:foreground "grey"))))
        ;; gt-overlay-render
        (after-source-overlay . ,(gt-translator
                                  :taker (gt-taker :text 'buffer :pick 'paragraph)
                                  :engines (gt-google-engine)
                                  :render (gt-overlay-render :type 'after
                                                             :sface nil
                                                             :rface 'font-lock-doc-face)))))

(setq gt-tts-native-engine 'edge-tts)

(defun gt--translate (dict)
  "Translate using DICT from the preset tranlators."
  (gt-start (alist-get dict gt-preset-translators)))

(defun gt-translate-prompt ()
  "Translate with prompt using the multiple dictionaries."
  (interactive)
  (gt--translate 'multi-dict))

(defun gt-use-text-utility ()
  "Handle the texts with the utilities."
  (interactive)
  (gt--translate 'Text-Utility))

(defun gt-use-korean-gt ()
  "Handle the texts with the utilities."
  (interactive)
  (gt--translate 'korean-gt))

;; (global-set-keys
;;  '(("C-c G"   . gt-translate-prompt)
;;    ("C-c d g" . gt-translate)
;;    ("C-c d G" . gt-translate-prompt)
;;    ("C-c d u" . gt-use-text-utility)))

(add-hook #'gt-buffer-render-output-hook  #'visual-line-mode)

(setq gt-source-text-transformer #'(lambda (text) (replace-regexp-in-string "[\r\n]" " " text)))

;; for embark
(with-eval-after-load 'embark
  (keymap-binds embark-region-map
    ("t" . gt-translate)))

(keymap-binds gt-overlay-render-map
  ("C-g" . gt-delete-render-overlays)
  ("<escape>" . gt-delete-render-overlays)
  ("M-w" . gt-overlay-render-save-to-kill-ring))

(provide 'init-gt)
;;; init-gt.el ends here
