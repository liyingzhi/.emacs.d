;;; init-custom.el --- Define customizations.        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defcustom user/birthday-dic nil
  "A dictionary to store user birthday information."
  :type '(alist :key-type symbol :value-type (cons integer integer))
  :group 'user)

(defcustom user/quickdir (list "~/" "~/Downloads/" "~/Documents/" "~/Music/" "~/MyProject/" "~/github/"  user-emacs-directory)
  "Quick dir list."
  :group 'user
  :type '(list dir))

;;; writter
(defcustom user/org-base-dir-path "~/Documents/Org"
  "Dir path for org mode file."
  :group 'user
  :type 'string)

(defcustom user/org-hugo-base-dir-path "~/MyProject/website/blog/"
  "Dir path for org-hugo export markdown file."
  :group 'user
  :type 'string)

(defcustom user/org-format-latex-options-scale 2.0
  "Set the org-format-latex-options scale value."
  :group 'user
  :type 'number)

;;; UI
(defcustom user/show-modeline t
  "Show modeline."
  :group 'user
  :type 'boolean)

(defcustom user/dashboard 'dashboard
  "Show dashboard."
  :group 'user
  :type '(choice (const :tag "dashboard" 'dashboard)
                 (const :tag "scratch" 'scratch)
                 (const :tag "enlight" 'enlight)))

(defcustom user/logo (file-truename
                      (concat user-emacs-directory
                              "logos/gnu_color.xpm"))
  "The Logo."
  :group 'user
  :type 'string)

(defcustom user/day-theme 'modus-operandi
  "Day theme name."
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'modus-vivendi
  "Night theme name."
  :group 'user
  :type 'symbol)

(defcustom user/start-fullscreen t
  "Is fullscreen in start."
  :group 'user
  :type 'boolean)

(defcustom user/*term-default-font* nil
  "Font name for term."
  :group 'user
  :type '(choice (const :tag "None" nil)
                 (string :tag "font name")))

(defcustom user/catppuccin-flavor 'mocha
  "Flavor for catppuccin theme."
  :group 'user
  :type 'symbol)

(defcustom a-theme-whitelist-light
  '(modus-operandi-tinted)
  "List of light themes available for random selection."
  :type '(repeat symbol)
  :group 'appearance)

(defcustom a-theme-whitelist-dark
  '(modus-vivendi-tinted)
  "List of dark themes available for random selection."
  :type '(repeat symbol)
  :group 'appearance)

(defcustom user/sidebar-magitblame nil
  "Use side bar to show commit id when magit-blame."
  :group 'user
  :type 'boolean)

(defcustom user/pretty-mode nil
  "Is Use pretty-mode."
  :group 'user
  :type 'boolean)

(defcustom user/hidden-outline t
  "Is hiden outline."
  :group 'user
  :type 'boolean)

;;; utils

(defcustom user/run-python-command "python"
  "A python command for Some package use python package."
  :group 'user
  :type 'string)

(defcustom user/completion-preview-mode-use nil
  "Is use `completion-preview-mode'."
  :group 'user
  :type 'boolean)

(defcustom user/load-eaf nil
  "Require init-eaf file."
  :group 'user
  :type 'boolean)

(defcustom user/eaf-python-command "python"
  "A python command for eaf use python package."
  :group 'user
  :type 'string)

(defcustom user/load-elfeed nil
  "Require init-elfeed file."
  :group 'user
  :type 'boolean)

(defcustom user/mms nil
  "Is use mms."
  :group 'user
  :type 'boolean)

(defcustom user/mms-playlist-file "~/Music/emms-playlist.pls"
  "Emms playlist file."
  :group 'user
  :type '(file :must-match t))

(defcustom user/telegap nil
  "Is use telega."
  :group 'user
  :type 'boolean)

(defcustom user/telega-start nil
  "Is auto start telega."
  :group 'user
  :type 'boolean)

(defcustom user/telega-tdlib-path "/usr/local"
  "Tdlib install path."
  :group 'user
  :type 'string)

(defcustom user/dirvish t
  "Drivish support."
  :group 'user
  :type 'boolean)

;;; Program
(defcustom user/lsp-client 'eglot
  "The lsp client."
  :group 'user
  :type '(choice (const :tag "eglot" eglot)
                 (const :tag "lsp-bridge" lsp-bridge)))

(defcustom user/flyoverp nil
  "Enable flyover."
  :group 'user
  :type 'boolean)

(defcustom user/move-style-motion t
  "Is Use cursor motion style when moving."
  :group 'user
  :type 'boolean)

(defcustom user/java-lsp nil
  "Is start java lsp for lsp-bridge."
  :group 'user
  :type 'boolean)

;;; AI
(defcustom user/qwen-models nil
  "List of available models for Qwen GPTel backend.
Each element should be a symbol representing a model name."
  :group 'user
  :type '(choice
          (const :tag "No models" nil)
          (list :tag "Model list" (repeat symbol))))

(defcustom user/modelscope-models '(deepseek-ai/DeepSeek-V3)
  "List of available models for modelscope GPTel backend.
Each element should be a symbol representing a model name."
  :group 'user
  :type '(choice
          (const :tag "No models" nil)
          (list :tag "Model list" (repeat symbol))))

(defcustom user/gt-chatgpt-model "qwen-turbo"
  "Model for gt-chatgpt-model qwen."
  :group 'user
  :type 'string)

(defcustom user/aider-deepseek-api t
  "Is use deepseek-api for aider ai."
  :group 'user
  :type 'boolean)

(defcustom user/gt-deepseek-api nil
  "Is use deepseek-api for go-translate ai."
  :group 'user
  :type 'boolean)

(defcustom user/ai-backend "deepseek"
  "Use what ai backend."
  :group 'user
  :type 'string)

(defcustom user/ai-model 'deepseek-chat
  "Use what ai model."
  :group 'user
  :type 'symbol)

(defcustom user/ai-backend-free "deepseek"
  "Use what ai backend."
  :group 'user
  :type 'string)

(defcustom user/ai-model-free 'deepseek-chat
  "Use what ai model."
  :group 'user
  :type 'symbol)

(defcustom user/ai-backend-coder "Qwen"
  "Use what ai backend for coder with tool calling."
  :group 'user
  :type 'string)

(defcustom user/ai-model-coder 'qwen3-coder-480b-a35b-instruct
  "Use what ai model to code."
  :group 'user
  :type 'symbol)

(defcustom user/aider nil
  "Aider support."
  :group 'user
  :type 'boolean)

;; Load `custom-file'
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(provide 'init-custom)
;;; init-custom.el ends here
