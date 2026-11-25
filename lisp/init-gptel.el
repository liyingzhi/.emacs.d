;;; init-gptel.el --- init gptel package             -*- lexical-binding: t; -*-

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

(require 'gptel)

(setq gptel-default-mode 'org-mode
      gptel-highlight-methods (if (controlling-tty-p) '(margin) '(fringe))
      gptel-track-media t
      gptel-expert-commands t
      gptel-prompt-prefix-alist nil
      gptel-response-prefix-alist nil)

(require 'gptel-org)
(setq-default gptel-org-branching-context t)

(defalias 'my/gptel-easy-page
  (let ((map (make-composed-keymap
              (define-keymap
                "RET" 'gptel-end-of-response
                "n"   'gptel-end-of-response
                "p"   'gptel-beginning-of-response)
              my-pager-map))
        (scrolling
         (propertize  "SCRL" 'face '(:inherit highlight))))
    (require 'pixel-scroll)
    (lambda ()
      (interactive)
      (when (eq (window-buffer (selected-window))
                (current-buffer))
        (add-to-list 'mode-line-format scrolling)
        (set-transient-map
         map t
         (lambda () (setq mode-line-format
                          (delete scrolling mode-line-format))))))))

(add-hook 'gptel-pre-response-hook #'my/gptel-easy-page)

;;; gptel state
;; Mode line status indicators
(add-to-list
 'global-mode-string
 '(:eval
   (when (local-variable-p 'gptel-system-prompt)
     (concat
      "["
      (if-let* ((n (car-safe (rassoc gptel-system-prompt gptel-directives))))
          (gptel--model-name n)
        (gptel--describe-directive gptel-system-prompt 12))
      "]"))))

(add-to-list
 'global-mode-string
 '(:eval
   (unless gptel-mode
     (when (and (local-variable-p 'gptel-model)
                (not (eq gptel-model (default-value 'gptel-model))))
       (concat "[" (gptel--model-name gptel-model) "]")))))

;; Variable to hold status indicator
(defvar-local gptel--mode-line-format " ")

(defun gptel--mode-line-format (msg &optional face)
  (setq gptel--mode-line-format
        (if face (propertize msg 'face face) msg)))

;; Update according to current state
(defun gptel--mode-line-update (fsm)
  (let ((buf (plist-get (gptel-fsm-info fsm) :buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (pcase (gptel-fsm-state fsm)
          ('WAIT (gptel--mode-line-format "↑" 'warning))
          ('TYPE (gptel--mode-line-format "↓" 'success))
          ('TOOL (gptel--mode-line-format "🔨" 'warning))
          ('ERRS (gptel--mode-line-format "❌" 'error))
          (_     (gptel--mode-line-format " ")))))))

;; Add to `gptel-send' FSM handlers
(dolist (state '(WAIT TYPE TOOL ERRS DONE))
  (cl-pushnew 'gptel--mode-line-update
              (alist-get state gptel-send--handlers)))

;; Add indicator to the `mode-line-misc-info'
(cl-pushnew '(:eval gptel--mode-line-format) global-mode-string
            :test #'equal)


;;; gptel backends
(gptel-make-openai "kimi"
  :stream t
  :protocol "https"
  :host "api.moonshot.cn"
  :key #'gptel-api-key
  :models '(moonshot-v1-8k moonshot-v1-128k))

(gptel-make-openai "llm-ollama"
  :stream t
  :protocol "https"
  :host "ollama.com"
  :key #'gptel-api-key
  :models '((gpt-oss:20b
             :capabilities (tool-use)
             :context-window 128)
            (gpt-oss:120b
             :capabilities (tool-use)
             :context-window 128)
            ))

(gptel-make-openai "fireworks"
  :stream t
  :protocol "https"
  :host "api.fireworks.ai"
  :endpoint "/inference/v1/chat/completions"
  :key #'gptel-api-key
  :models '(accounts/fireworks/models/llama-v3p1-405b-instruct))

(gptel-make-deepseek "deepseek"
  :stream t
  :key #'gptel-api-key)

(gptel-make-gemini "Gemini"
  :stream t
  :key #'gptel-api-key)

(gptel-make-openai "Chatanywhere"
  :stream t
  :protocol "https"
  :host "api.chatanywhere.tech"
  :key #'gptel-api-key
  :models '(deepseek-chat deepseek-ocr deepseek-r1 deepseek-v3.2 deepseek-v3.2-thinking gpt-4o-ca gpt-4.1-ca gpt-5.1-ca gpt-5-chat-latest-ca gpt-5.1-chat-latest-ca))

(gptel-make-anthropic "Claude"
  :stream t
  :host "api.openai-proxy.org/anthropic"
  :key #'gptel-api-key)

(gptel-make-openai "Qwen"
  :stream t
  :protocol "https"
  :host "dashscope.aliyuncs.com"
  :endpoint "/compatible-mode/v1/chat/completions"
  :key #'gptel-api-key
  :models user/qwen-models)

(gptel-make-openai "local-llm-backend"
  :stream t
  :host "localhost:8080"
  :endpoint "/v1/chat/completions"
  :protocol "http"
  :models user/local-models)

(gptel-make-openai "local-llm-Rl2"
  :stream t
  :host user/local-llm-server-host
  :endpoint "/v1/chat/completions"
  :protocol "http"
  :key #'gptel-api-key
  :models user/local-models)

(gptel-make-openai "ModelScope"
  :stream t
  :protocol "https"
  :host "api-inference.modelscope.cn"
  :key #'gptel-api-key
  :models user/modelscope-models)

;; OpenRouter offers an OpenAI compatible API
(gptel-make-openai "OpenRouter"               ;Any name you want
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :stream t
  :key #'gptel-api-key                   ;can be a function that returns the key
  :models '(cognitivecomputations/dolphin3.0-r1-mistral-24b:free
            google/gemini-2.5-pro-exp-03-25:free
            deepseek/deepseek-r1:free
            deepseek/deepseek-chat-v3-0324:free))

(gptel-make-anthropic "MiniMax"
  :stream t
  :protocol "https"
  :host "api.minimaxi.com"
  :endpoint "/anthropic/v1/messages"
  :key #'gptel-api-key
  :models '(MiniMax-M2.5 MiniMax-M2.5-highspeed MiniMax-M2.7 MiniMax-M2.7-highspeed))

(setq gptel-backend
      (gptel-get-backend user/ai-backend-free))
(setq gptel-model user/ai-model-free)

;; (setq gptel-model 'deepseek/deepseek-r1:free)
;; (setq gptel-model 'moonshot-v1-8k)
;; (setq gptel-model 'claude-3-5-sonnet-20241022)

(defun my/switch-gptel-llm ()
  (interactive)
  (if (equal gptel-model user/ai-model-free)
      (progn (setq gptel-backend (gptel-get-backend user/ai-backend))
             (setq gptel-model user/ai-model)
             (setq gptel-use-tools t))
    (setq gptel-backend
          (gptel-get-backend user/ai-backend-free))
    (setq gptel-model user/ai-model-free)
    (setq gptel-use-tools nil)))

(defun my/switch-gptel-llm-coder ()
  (interactive)
  (when (not (equal gptel-model user/ai-model-coder))
    (setq gptel-backend
          (gptel-get-backend user/ai-backend-coder))
    (setq gptel-model user/ai-model-coder)
    (setq gptel-use-tools t)))

;;; gptel prompts
(require 'agental-prompts)
(add-to-list 'agental-prompts-path
             (concat user-emacs-directory
                     "config/prompts"))

(setopt agental-tool-show-tool-call 'auto
        agental-tool-include nil)

(agental-install (list :backend user/ai-backend
                       :model user/ai-model))

(add-list-to-list 'gptel-directives
                  `((translate . ,(concat "You are a large language model and a writing assistant. Respond concisely."
                                          "  Follow my instructions and improve or rewrite the text I provide."
                                          "  Generate ONLY the replacement text,"
                                          " without any explanation or markdown code fences or org code fences."
                                          " translate chinese to english."))
                    (docstr . ,(agental-prompts-make 'docstr nil))
                    (ragmacs . ,(agental-prompts-make 'ragmacs nil))
                    (translate-english . ,(agental-prompts-make 'translate
                                                                '(("to" . "english")
                                                                  ("user_name" . "lizqwerscott"))))))

;;; gptel tools
(require 'init-gptel-tools)

(defun gptel-translate-to-english (&optional dry-run)
  "Use AI to translate the currently selected text into English.

The DRY-RUN parameter is set to t, indicating that it will not actually run, but only simulate the run."
  (interactive "P")
  (unless (or gptel--rewrite-overlays (use-region-p))
    (user-error "`gptel-translate-to-english' requires an active region or rewrite in progress."))
  (let ((gptel-tools nil)
        (gptel-use-tools nil))
    (gptel-request (list (or (get-char-property (point) 'gptel-rewrite)
                             (buffer-substring-no-properties (region-beginning) (region-end)))
                         "What is the required change?"
                         "Rewrite:")
      :dry-run dry-run
      :system (alist-get 'translate gptel-directives)
      :stream t
      :context
      (let ((ov (or (cdr-safe (get-char-property-and-overlay (point) 'gptel-rewrite))
                    (make-overlay (region-beginning) (region-end) nil t))))
        (overlay-put ov 'category 'gptel)
        (overlay-put ov 'evaporate t)
        (cons ov (generate-new-buffer "*gptel-rewrite*")))
      :callback #'gptel--rewrite-callback)))

(defun gptel-translate-to-english-insert (str)
  "Use AI to translate the STR into English."
  (interactive (list (read-string "Input: " nil nil nil default-input-method)))
  (let ((buffer (current-buffer))
        (pos (point))
        (gptel-tools nil)
        (gptel-use-tools nil))
    (gptel-request str
      :dry-run nil
      :system (alist-get 'translate-english gptel-directives)
      :stream t
      :callback (lambda (res info)
                  (message "res: %s" res)
                  (with-current-buffer buffer
                    (save-excursion
                      (goto-char pos)
                      (cond
                       ((stringp res)
                        (insert res))
                       ((eq res 'abort)
                        (message "error"))
                       ((null res)
                        (message "LLM respone error"))
                       ((consp res))
                       (t
                        (message "Translated finish")))
                      (setq pos (point))))))))

(with-eval-after-load 'gptel-transient
  (transient-append-suffix 'gptel-menu '(2 -1)
    ["Quick Tools"
     ("q t" "Translate select regions to english" gptel-translate-to-english)
     ("q q" "gptel abort" gptel-abort)]))

;;; gptel preset
(require 'gptel-preset-collection)
(advice-add 'gptel-preset-capf :after-until #'gptel-preset-collection-capf)

(gptel-make-preset 'default
  :description "Default"
  :system (alist-get 'default gptel-directives)
  :backend user/ai-backend-free
  :model user/ai-model-free
  :tools nil
  :use-tools nil)

(let ((agent (alist-get 'emacs-agent agental-prompts-templates)))
  (gptel-make-preset 'emacs
    :description (plist-get agent :description)
    :pre (lambda () (require 'ragmacs))
    :system (plist-get agent :system)
    :backend user/ai-backend
    :model user/ai-model
    :tools (plist-get agent :tools)
    :use-tools t))

(gptel-make-preset 'elisp-document
  :description "Elisp 文档大师"
  :system (alist-get 'docstr gptel-directives))

;; program agent without tool and answer with Zh-CN
(let ((agent (alist-get 'program-agent-wo-tool agental-prompts-templates)))
  (gptel-make-preset 'program-wo-tool
    :description (plist-get agent :description)
    :system (plist-get agent :system)
    :backend user/ai-backend-free
    :model user/ai-model-free
    :use-tools nil))

;; ragmacs
(gptel-make-preset 'ragmacs
  :description "Ragmacs"
  :pre (lambda () (require 'ragmacs))
  :system (alist-get 'ragmacs gptel-directives)
  :backend user/ai-backend
  :model user/ai-model
  :tools '("introspection")
  :use-tools t)

;;; gptel-quick
(require 'gptel-quick)
(setq gptel-quick-system-message
      #'(lambda (count)
          (let* ((lang (downcase (gptel--strip-mode-suffix major-mode)))
                 (article (if (and lang (not (string-empty-p lang))
                                   (memq (aref lang 0) '(?a ?e ?i ?o ?u)))
                              "an" "a")))
            (if (derived-mode-p 'prog-mode)
                (format (concat "You are %s %s programmer.  "
                                "Explain in %d words or fewer."
                                "It is best to use Chinese for the explanation.")
                        article lang count)
              (concat
               (if (string-empty-p lang)
                   "You are an editor."
                 (format "You are %s %s editor." article lang))
               (format "Explain in %d words or fewer." count)
               "It is best to use Chinese for the explanation.")))))

(global-set-keys
 '((("M-?" "s-?") . gptel-quick)))

;; insert file into gptel content
(defun my/org-link-file (file &optional arg)
  "Generate an Org-mode file link from FILE path.

When the current `default-directory' can be used as a relative root,
a relative file link (e.g., \"file:relative/path\") is produced.
Otherwise, an absolute file link is returned.

With optional ARG equal to \\='(16) (i.e., \\='\\='\\=' prefix),
the link uses `abbreviate-file-name' instead of trying to make it relative."
  (let ((pwd (file-name-as-directory (expand-file-name ".")))
	    (pwd1 (file-name-as-directory (abbreviate-file-name
				                       (expand-file-name ".")))))
    (cond ((equal arg '(16))
	       (concat "file:"
		           (abbreviate-file-name (expand-file-name file))))
	      ((string-match
	        (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
	       (concat "file:" (match-string 1 file)))
	      ((string-match
	        (concat "^" (regexp-quote pwd) "\\(.+\\)")
	        (expand-file-name file))
	       (concat "file:"
		           (match-string 1 (expand-file-name file))))
	      (t (concat "file:" file)))))

(defun my/gptel-context-add-file-with-current-kill-ring (arg)
  "Add file from `kill-ring' to gptel context.

With \\='(4) prefix ARG (i.e., \\='C-u'), insert an Org-mode file link
into the current buffer instead of adding the file contents directly.

When called without prefix, delegate to `gptel-add-file' to include
the file content as context.

Signal `user-error' if `kill-ring' is empty, or if the current
kill-ring entry is not a string representing an existing file path."
  (interactive "P")
  (if (not kill-ring)
      (user-error "Kill ring is empty")
    (let ((file (current-kill 0 'do-not-move)))
      (if (and (stringp file) (file-exists-p file))
          (if arg
              (prog1
                  (insert (org-link-make-string-for-buffer (my/org-link-file file)
                                                           nil
                                                           nil))
                (setq gptel-track-media t))
            (gptel-add-file file))
        (user-error "Current kill ring is not a valid real file path")))))

;;; gptel-rewrite
(with-eval-after-load 'gptel-rewrite
  (defun gptel--rewrite-inline-diff (&optional ovs)
    (interactive (list (gptel--rewrite-overlay-at)))
    (unless (require 'inline-diff nil t)
      (user-error "Inline diffs require the inline-diff package"))
    (when-let* ((ov-buf (overlay-buffer (or (car-safe ovs) ovs)))
                ((buffer-live-p ov-buf)))
      (with-current-buffer ov-buf
        (cl-loop for ov in (ensure-list ovs)
                 for ov-beg = (overlay-start ov)
                 for ov-end = (overlay-end ov)
                 for response = (overlay-get ov 'gptel-rewrite)
                 do (delete-overlay ov)
                 (inline-diff-words
                  ov-beg ov-end response)))))
  (keymap-sets gptel-rewrite-actions-map
    '(("C-c C-i" . gptel--rewrite-inline-diff))))

;;; gptel-annotate
(require 'gptel-annotate)
(put 'gptel-annotate-flymake 'flymake-always-safe t)

;;; gptel keymap and hooks
(defun my/gptel-assign-filename ()
  (unless (or (buffer-file-name) current-prefix-arg)
    (make-directory my/gptel-chat-directory t)
    (my/gptel-mode-add-prop-line)
    (setq buffer-file-name
          (file-name-concat
           my/gptel-chat-directory
           (concat
            (format-time-string "%Y%m%d%H%M%2S-")
            (file-name-sans-extension
             (replace-regexp-in-string " " "-" (buffer-name)))
            (pcase major-mode
              ('org-mode ".org") ('markdown-mode ".md") (_ ".txt")))))
    (rename-buffer (file-name-nondirectory buffer-file-name) t)))

(keymap-binds gptel-mode-map
  ("C-o" . gptel-menu)
  (("C-s-p" "C-M-p") . my/gptel-context-add-file-with-current-kill-ring)
  ("C-c SPC" . my/gptel-easy-page))

(defun my/gptel-remove-headings (beg end)
  (when (and (derived-mode-p 'org-mode)
             gptel-org-branching-context)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward org-heading-regexp end t)
        (forward-line 0)
        (delete-char (1+ (length (match-string 1))))
        (insert-and-inherit "*")
        (end-of-line)
        (skip-chars-backward " \t\r")
        (insert-and-inherit "*")))))

(defun my/gptel-mode-add-prop-line ()
  "Ensure that this file opens with `gptel-mode' enabled."
  (save-excursion
    (let ((enable-local-variables t)) ; Ensure we can modify local variables
      (if (and (save-excursion (goto-char (point-min)) (looking-at ".*-\\*-")))
          ;; If there's a -*- line
          ;; First remove any existing eval, then add the new one
          (modify-file-local-variable-prop-line 'eval nil 'delete))
      ;; Always add our eval
      (add-file-local-variable-prop-line
       'eval '(and (fboundp 'gptel-mode) (gptel-mode 1))))))

(defvar my/gptel-chat-directory "~/Documents/gptel/")

(defun gptel-resume (chat)
  "Resume previous gptel chat stored in `my/gptel-chat-directory'."
  (interactive (list (read-file-name
                      "Resume chat: " my/gptel-chat-directory nil t)))
  (find-file chat))

(add-hook 'gptel-mode-hook
          #'gptel-highlight-mode)

(add-hook 'gptel-mode-hook
          #'(lambda ()
              (when (bound-and-true-p org-latex-preview-mode)
                (org-latex-preview-mode -1))
              (when (bound-and-true-p ratex-mode)
                (ratex-mode -1))))

(with-hook gptel-mode
  (setq-local gptel-cache t)
  (add-hook 'before-save-hook #'my/gptel-assign-filename
            nil 'local))

(add-hook 'gptel-post-response-functions #'my/gptel-remove-headings)

;;; gptel-agent
;; Refs: https://github.com/karthink/.emacs.d
(require 'gptel-agent)

(gptel-agent-update)

(setq gptel-agent-preset nil)

(defvar my/gptel-agent-edit-confirm-cache nil)

(defun my/gptel-agent-edit-or-insert-confirm (path &rest _args)
  "Don't ask for confirmation if path is git-controlled.
Edit freely."
  (not
   (with-memoization (alist-get path my/gptel-agent-edit-confirm-cache
                                nil nil #'equal)
     (and (file-readable-p path)
          ;; TODO Also check if path is part of current project
          (locate-dominating-file path ".git")
          (eql (call-process "git" nil nil nil
                             "ls-files" "--error-unmatch" path)
               0)))))

(setf (gptel-tool-confirm (gptel-get-tool "Edit"))
      #'my/gptel-agent-edit-or-insert-confirm
      (gptel-tool-confirm (gptel-get-tool "Insert"))
      #'my/gptel-agent-edit-or-insert-confirm)

(gptel-make-preset 'skill
  :description "TOOLS: Add skill-reading tool"
  :pre (lambda () (require 'gptel-agent-tools))
  :tools '(:append ("Skill"))
  :system '(:function
            (lambda (sys)
              (concat sys "\n\n" (gptel-agent--skills-system-message
                                  (gptel-agent--update-skills))))))
(gptel-make-preset 'web
  :description "TOOLS: Add basic web search tools"
  :pre (lambda () (require 'gptel-agent-tools))
  :tools '(:append ("WebSearch" "WebFetch" "YouTube"))
  ;; :system '(:append "\n\nUse the provided tools to search the web for up-to-date information.")
  )
(gptel-make-preset 'files
  :pre (lambda () (require 'gptel-agent-tools))
  :description "TOOLS: Add file read/write"
  :tools '(:append ("Read" "Glob" "Write" "Edit" "Insert")))
(gptel-make-preset 'files-ro
  :pre (lambda () (require 'gptel-agent-tools))
  :description "TOOLS: Add file read-only"
  :tools '(:append ("Read" "Glob")))
(gptel-make-preset 'shell
  :pre (lambda () (require 'gptel-agent-tools))
  :description "TOOLS: Add Bash eval"
  :tools  '(:append ("Bash"))
  ;; :system '(:append "Use the Bash tool to introspect and change the state of the system.")
  )
(gptel-make-preset 'eval
  :pre (lambda () (require 'gptel-agent-tools))
  :tools  '(:append ("Eval"))
  :system '(:append "Use the Eval tool to change the state of the running Emacs instance.")
  :description "TOOLS: Add eval")
(gptel-make-preset 'introspect
  :pre (lambda () (require 'gptel-agent-tools-introspection))
  :description "Introspect Emacs with Ragmacs"
  :system
  "You are pair programming with the user in Emacs and on Emacs.

Your job is to dive into Elisp code and understand the APIs and
structure of elisp libraries and Emacs.  Use the provided tools to do
so, but do not make duplicate tool calls for information already
available in the chat.

<tone>
1. Be terse and to the point.  Speak directly.
2. Explain your reasoning.
3. Do NOT hedge or qualify.
4. If you don't know, say you don't know.
5. Do not offer unprompted advice or clarifications.
6. Never apologize.
7. Do NOT summarize your answers.
</tone>

<code_generation>
When generating code:
1. Create a plan first: list briefly the design steps or ideas involved.
2. Use the provided tools to check that functions or variables you use
in your code exist.
3. Also check their calling convention and function-arity before you use
them.
</code_generation>

<formatting>
1. When referring to code symbols (variables, functions, tags etc)
enclose them in markdown quotes.
  Examples: `read_file`, `getResponse(url, callback)`
  Example: `<details>...</details>`
2. If you use LaTeX notation, enclose math in \( and \), or \[ and \] delimiters.
</formatting>"
  :cache '(tool)
  :tools '("introspection"))

;;; gptel aibo
(require 'init-gptel-aibo)

;;; gptel-magit
(add-hook 'magit-mode-hook #'gptel-magit-install)

;; gptel-magit self-handling does not include thinking content.
;; (gptel--request-params user/gptel-close-thinking)

(with-eval-after-load 'gptel-magit
  (defun gptel-no-tool-wrap (orig-fn &rest args)
    "Let ORIG-FN not use tools.
ARGS is ORIG-FN args."
    (let ((gptel-tools nil)
          (gptel-use-tools nil))
      (apply orig-fn args)))

  (advice-add #'gptel-magit--request :around #'gptel-no-tool-wrap))

(provide 'init-gptel)
;;; init-gptel.el ends here
