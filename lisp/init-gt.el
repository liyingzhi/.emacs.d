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

(setopt gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . right)
          (window-width . 0.35)))

(with-eval-after-load 'gt
  (require 'lib-gt)

  (setopt gt-tts-native-engine 'edge-tts)

  (setopt gt-source-text-transformer #'(lambda (text) (replace-regexp-in-string "[\r\n]" " " text)))

  (add-hook #'gt-buffer-render-output-hook  #'visual-line-mode)

  (if user/aider-deepseek-api
      (let* ((info (lizqwer/api-key-from-auth-source "deepseek.com")))
        (setopt gt-chatgpt-key info)
        (setopt gt-chatgpt-host "https://api.deepseek.com")
        (setopt gt-chatgpt-path "/chat/completions")
        (setopt gt-chatgpt-model "deepseek-chat")
        (setq my-ai-oneshot-models (list "deepseek-chat" "deepseek-reasoner")))
    (let* ((info (lizqwer/api-key-from-auth-source "dashscope.aliyuncs.com")))
      (setopt gt-chatgpt-key info)
      (setopt gt-chatgpt-host "https://dashscope.aliyuncs.com")
      (setopt gt-chatgpt-path "/compatible-mode/v1/chat/completions")
      (setopt gt-chatgpt-model user/gt-chatgpt-model)
      (setq my-ai-oneshot-models (mapcar #'symbol-name user/qwen-models))))

  (setopt gt-preset-translators
          `((default . ,(gt-translator
                         :taker   (list (gt-taker :pick nil :if 'selection)
                                        (gt-taker :text 'paragraph :if #'gt--taker-paragraph-p)
                                        (gt-taker :text 'sentence :if #'gt--at-line-beginning-p)
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
            (simple-dict . ,(gt-translator :taker (gt-taker :prompt t :pick nil :text nil)
                                           :engines (list (gt-google-engine :if '(and not-word))
                                                          (gt-youdao-suggest-engine :if 'word))
                                           :render (gt-buffer-render)))
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
  (keymap-binds gt-overlay-render-map
    ("C-g" . gt-delete-render-overlays)
    ("<escape>" . gt-delete-render-overlays)
    ("M-w" . gt-overlay-render-save-to-kill-ring)))

;; (global-set-keys
;;  '(("C-c G"   . gt-translate-prompt)
;;    ("C-c d g" . gt-translate)
;;    ("C-c d G" . gt-translate-prompt)
;;    ("C-c d u" . gt-use-text-utility)))

(autoload #'gt-translate-prompt "lib-gt" nil t)
(autoload #'gt-translate-prompt-with-simple-dict "lib-gt" nil t)
(autoload #'gt-use-korean-gt "lib-gt" nil t)
(autoload #'gt-use-text-utility "lib-gt" nil t)
(autoload #'my/gt-ai-oneshot "lib-gt" nil t)

;; for embark
(with-eval-after-load 'embark
  (keymap-binds embark-region-map
    ("t" . gt-translate)))

(provide 'init-gt)
;;; init-gt.el ends here
