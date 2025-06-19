;;; init-aider.el --- init aider package             -*- lexical-binding: t; -*-

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

(require 'aidermacs)

(if user/aider-deepseek-api
    (let* ((info (lizqwer/api-key-from-auth-source "deepseek.com")))
      (setenv "DEEPSEEK_API_KEY" info)
      (setq aidermacs-show-diff-after-change t
            aidermacs-use-architect-mode t
            aidermacs-default-model "deepseek/deepseek-reasoner"
            aidermacs-architect-model "deepseek/deepseek-reasoner"
            aidermacs-editor-model "deepseek/deepseek-chat"
            aidermacs-extra-args (list "--chat-language" "zh-cn")
            ;; aidermacs-backend 'vterm
            ))
  (let* ((info (lizqwer/api-key-from-auth-source "dashscope.aliyuncs.com")))
    (setenv "OPENAI_API_KEY" info)
    (setenv "OPENAI_API_BASE" "https://dashscope.aliyuncs.com/compatible-mode/v1")
    (setq aidermacs-show-diff-after-change t
          aidermacs-use-architect-mode t
          aidermacs-default-model "openai/deepseek-r1"
          aidermacs-architect-model "openai/deepseek-r1"
          aidermacs-editor-model "openai/deepseek-v3"
          aidermacs-extra-args (list "--chat-language" "zh-cn"))))

(unless aidermacs--read-string-history
  (setq aidermacs--read-string-history aidermacs-common-prompts))

(keymap-set aidermacs-comint-mode-map
            "C-M-o"
            #'aidermacs-transient-menu)

(provide 'init-aider)
;;; init-aider.el ends here
