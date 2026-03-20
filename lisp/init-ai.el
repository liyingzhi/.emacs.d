;;; init-ai.el --- init ai package                   -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp, ai

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

(require 'init-gptel)

(require 'init-macher)

(defun set-ai-completion (symbol value)
  "Set font SYMBOL VALUE."
  (dolist (mode '(python-ts-mode rust-ts-mode c++-ts-mode web-mode bash-ts-mode go-ts-mode csharp-mode csharp-ts-mode))
    (remove-hook (intern (concat (symbol-name mode) "-hook"))
                 #'copilot-mode))
  (remove-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
  (remove-hook 'prog-mode-hook #'wingman-mode)
  (set-default-toplevel-value symbol value)
  (wait-packages!
   (pcase value
     ('copilot
      '(copilot))
     ('minuet
      '((minuet :host github
                :repo "milanglacier/minuet-ai.el")))
     ('wingman
      '((wingman :host github :repo "mjrusso/wingman")))))
  (pcase value
    ('copilot
     (require 'init-copilot))
    ('minuet
     (require 'init-minuet-ai))
    ('wingman
     (add-hook 'prog-mode-hook
               #'wingman-mode))))

(defcustom user/ai-completion nil
  "Use what ai to completion: copilot, minuet, wingman."
  :group 'user
  :type '(choice (const :tag "none" nil)
                 (const :tag "copilot" copilot)
                 (const :tag "minuet" minuet)
                 (const :tag "wingman" wingman))
  :set #'set-ai-completion)

(defun ai-complete ()
  "Complete with the ai in corfu."
  (pcase user/ai-completion
    ('copilot
     (copilot-accept-completion))
    ('minuet
     (minuet-accept-suggestion))
    ('wingman
     (wingman-accept-full))))

;;; agent-shell
(require 'acp)
(require 'agent-shell)

(setq agent-shell-file-completion-enabled t
      agent-shell-qwen-authentication (agent-shell-qwen-make-authentication :login t))

(keymap-sets agent-shell-mode-map
  '(("C-o" . agent-shell-help-menu)
    ("C-c RET" . shell-maker-submit)
    ("RET" . newline)))

;;; ai-code-interface

(when user/ai-code-interface

  (wait-packages!
   '(ai-code))

  (with-eval-after-load 'ai-code
    ;; use codex as backend, other options are 'claude-code, 'gemini, 'github-copilot-cli, 'opencode,
    ;; 'grok, 'cursor, 'kiro, 'codebuddy, 'aider, 'eca, 'agent-shell, 'claude-code-ide, 'claude-code-el
    (ai-code-set-backend 'opencode)

    ;; Optional: Use eat if you prefer, by default it is vterm
    ;; config for native CLI backends. for external backends such as agent-shell, claude-code-ide.el and claude-code.el, please check their own config
    ;; (setopt ai-code-backends-infra-terminal-backend 'eat)

    ;; Optional: Enable @ file completion in comments and AI sessions
    ;; (ai-code-prompt-filepath-completion-mode 1)

    ;; Optional: Ask AI to run test after code changes, for a tighter build-test loop
    (setopt ai-code-auto-test-type nil)
    (setopt ai-code-prompt-suffix "Only use English in code file, but Reply in Simplified Chinese language"))

  ;; Optional: Set up Magit integration for AI commands in Magit popups
  (with-eval-after-load 'magit
    (ai-code-magit-setup-transients)))



;;; mcp
(require 'init-mcp)

(provide 'init-ai)
;;; init-ai.el ends here
