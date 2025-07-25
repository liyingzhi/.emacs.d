;;; init-const.el --- some const file                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defcustom user/use-proxy nil
  "The proxy host of all."
  :group 'user
  :type 'boolean)

(defcustom user/proxy-host "127.0.0.1"
  "The proxy host of all."
  :group 'user
  :type 'string)

(defcustom user/proxy-all-port 20170
  "The all proxy port of all"
  :group 'user
  :type 'number)

(defcustom user/proxy-http-port 20171
  "The http proxy port of all"
  :group 'user
  :type 'number)

(defcustom user/proxy-rule-port 20172
  "The rule proxy  port of all"
  :group 'user
  :type 'number)

(defcustom user/show-modeline t
  "Show modeline"
  :group 'user
  :type 'boolean)

(defcustom user/dashboard t
  "Show dashboard"
  :group 'user
  :type 'boolean)

(defcustom user/logo (file-truename
                      (concat user-emacs-directory
                              "logos/gnu_color.xpm"))
  "Show dashboard"
  :group 'user
  :type 'string)

(defcustom user/day-theme 'ef-spring
  "User day theme"
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'doom-dracula
  "User night theme"
  :group 'user
  :type 'symbol)

(defcustom user/start-fullscreen t
  "Is fullscreen in start"
  :group 'user
  :type 'boolean)

(defcustom user/start-transparent t
  "Is transparent in start"
  :group 'user
  :type 'boolean)


(defcustom user/sidebar-magitblame nil
  "use side bar to show commit id when magit-blame"
  :group 'user
  :type 'boolean)

(defcustom user/load-eaf nil
  "require init-eaf file"
  :group 'user
  :type 'boolean)

(defcustom user/load-elfeed nil
  "require init-elfeed file"
  :group 'user
  :type 'boolean)

(defcustom user/run-python-command "python"
  "A python command for Some package use python package"
  :group 'user
  :type 'string)

(defcustom user/eaf-python-command "python"
  "A python command for Some package use python package"
  :group 'user
  :type 'string)

(defcustom user/completion-preview-mode-use nil
  "Is use `completion-preview-mode'"
  :group 'user
  :type 'boolean)

(defcustom user/vivaldi-use nil
  "Is use vivaldi"
  :group 'user
  :type 'boolean)

(defcustom user/telega-start nil
  "Is start telega"
  :group 'user
  :type 'boolean)

(defcustom user/flyover-start nil
  "Is start flyover"
  :group 'user
  :type 'boolean)

(defcustom user/telega-tdlib-path "/usr/local"
  "tdlib install path"
  :group 'user
  :type 'string)

(defcustom user/java-lsp nil
  "Is start java lsp"
  :group 'user
  :type 'boolean)

(defcustom user/ai-completion nil
  "use what ai to completion: codeium, copilot, minuet"
  :group 'user
  :type 'symbol)

(defcustom user/qwen-models nil
  "List of available models for Qwen GPTel backend.
Each element should be a symbol representing a model name."
  :group 'user
  :type '(choice
          (const :tag "No models" nil)
          (list :tag "Model list" (repeat symbol))))

(defcustom user/gt-chatgpt-model "qwen-turbo"
  "model for gt-chatgpt-model qwen"
  :group 'user
  :type 'string)

(defcustom user/aider nil
  "Is start aider ai"
  :group 'user
  :type 'boolean)

(defcustom user/aider-deepseek-api t
  "Is use deepseek-api for aider ai"
  :group 'user
  :type 'boolean)

(defcustom user/gt-deepseek-api nil
  "Is use deepseek-api for go-translate ai"
  :group 'user
  :type 'boolean)

;;;###autoload
(defun get-socks-proxy ()
  (concat "socks5://"
          user/proxy-host
          ":"
          (format "%d" user/proxy-all-port)))

;;;###autoload
(defun get-http-proxy ()
  (concat "http://"
          user/proxy-host
          ":"
          (format "%d" user/proxy-http-port)))

;;;###autoload
(defun get-url-proxy ()
  `(("http" . ,(concat
                user/proxy-host
                ":"
                (format "%d" user/proxy-http-port)))
    ("https" . ,(concat
                 user/proxy-host
                 ":"
                 (format "%d" user/proxy-http-port)))))

(defcustom user/font-mac-size 230
  "The font size in mac"
  :group 'user
  :type 'number)

(defcustom user/font-win-size 110
  "The font size in window"
  :group 'user
  :type 'number)

(defcustom user/font-linux-size 190
  "The font size in linux"
  :group 'user
  :type 'number)

(defcustom user/lsp-client 'lsp-bridge
  "The lsp client"
  :group 'user
  :type '(choice (const :tag "eglot" eglot)
                 (const :tag "lsp-bridge" lsp-bridge)))

(defcustom user/dirvish t
  "Is Use dirvish"
  :group 'user
  :type 'boolean)

(defcustom user/birthday-dic nil
  "A dictionary to store user birthday information."
  :type '(alist :key-type symbol :value-type (cons integer integer))
  :group 'user)

(defcustom user/ai-backend "deepseek"
  "use what ai backend"
  :group 'user
  :type 'string)

(defcustom user/ai-model 'deepseek-chat
  "use what ai model"
  :group 'user
  :type 'symbol)

(defcustom user/ai-backend-free "deepseek"
  "use what ai backend"
  :group 'user
  :type 'string)

(defcustom user/ai-model-free 'deepseek-chat
  "use what ai model"
  :group 'user
  :type 'symbol)

(defcustom user/ai-model-coder 'qwen3-coder-480b-a35b-instruct
  "use what ai model to code"
  :group 'user
  :type 'symbol)

(defcustom user/move-sytle-motion t
  "Is Use cusor motion style when moving"
  :group 'user
  :type 'boolean)

(defcustom user/ligature nil
  "Is Use ligature"
  :group 'user
  :type 'boolean)

(defcustom user/pretty-mode nil
  "Is Use pretty-mode"
  :group 'user
  :type 'boolean)

(defcustom user/unity nil
  "Is Use unity"
  :group 'user
  :type 'boolean)

(provide 'init-const)
;;; init-const.el ends here.
