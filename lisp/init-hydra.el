;;; init-hydra.el --- init hydra                     -*- lexical-binding: t; -*-

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

(require 'lib-hydra)
(pretty-hydra-define-e hydra-toggles
  (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on") :color amaranth :quit-key ("C-g" "q" "<escape>"))
  ("Basic"
   (("w" toggle-sub-word-or-super-word "sub or super word" :toggle (bound-and-true-p subword-mode))
    ("e" electric-pair-mode "electric pair" :toggle t)
    ("s" super-save-mode "auto save" :toggle t)
    ("a" global-aggressive-indent-mode "aggressive indent" :toggle t)
    ("c" global-centered-cursor-mode "centered cursor" :toggle t)
    ("i" immersive-translate-auto-mode "immersive translate" :toggle t)
    ("t" +lizqwer/toggle-telega "telega" :toggle (get-buffer "*Telega Root*"))
    ("l" interaction-log-mode "interactive log" :toggle t))
   "Highlight"
   (("h l" global-hl-line-mode "line" :toggle t)
    ("h p" show-paren-mode "paren" :toggle t)
    ("h s" symbol-overlay-mode "symbol" :toggle t)
    ("h r" colorful-mode "colorful" :toggle t)
    ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace)) "whitespace" :toggle show-trailing-whitespace)
    ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
    ("h i" indent-bars-mode "indent" :toggle t))
   "Ui"
   (("n" display-line-numbers-mode "line number" :toggle t)
    ("d" +lizqwer/toggle-dark-theme "dark theme" :toggle (cl-find user/night-theme custom-enabled-themes))
    ("T" +lizqwer/toggle-transparent "transparent" :toggle (not (eq (frame-parameter (selected-frame) 'alpha-background) 100)))
    ("r" a-random-theme "rand theme")
    ("R" redacted-mode "redacted" :toggle t)
    ("b" imenu-list-smart-toggle "imenu list" :toggle imenu-list-minor-mode)
    ("k" keycast-header-line-mode "keycast" :toggle t)
    ("o" outli-mode "Outline" :toggle t))
   "Program"
   (("v" global-diff-hl-mode "diff-hl mode" :toggle t)
    ("M" diff-hl-margin-mode "margin gutter" :toggle t)
    ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
    ("m" +lizqwer/toggle-move-style "move style" :toggle user/move-style-motion))
   "Customize"
   (("S" customize-save-variable "Customize save variable" :exit t)
    ("C" customize-set-variable "Customize set variable" :exit t))
   "LLM"
   (("G f" my/switch-gptel-llm "llm free" :toggle (equal gptel-model user/ai-model-free))
    ("G c" my/switch-gptel-llm-coder "llm coder" :toggle (equal gptel-model user/ai-model-coder)))))

(global-set-keys
 '(("C-c T" . hydra-toggles/body)
   ("<f6>" . hydra-toggles/body)))

(pretty-hydra-define-e hydra-jump-dir
  (:title (pretty-hydra-title "Jump to directory" 'octicon "nf-oct-file_directory_open_fill") :color amaranth :quit-key ("C-g" "q" "<escape>") :all-exit t)
  ("Base"
   (("t" trashed "Trashed")
    ("d" consult-dir "Dirs")

    ("n"  (lambda ()
            (interactive)
            ;; (dired (denote-directories))
            (denote-sort-dired ".*" denote-sort-dired-default-sort-component t nil)
            ) "Denote-Dir")

    ("v" (lambda ()
           (interactive)
           (when user/dirvish
             (call-interactively #'dirvish-dwim))) "Dirvish"))

   "Search"
   (("s s" (lambda ()
             (interactive)
             (autoload 'consult-fd-dir "init-func" nil t)
             (consult-fd-dir)) "Fuzzy search dir HOME")
    ("s n" consult-notes "Fuzzy search dir Note")
    ("s d" consult-denote-find "Fuzzy search dir Denote")
    ("j" dired-jump "Dired jump")
    ("J" dired-jump-other-window "Dired jump other"))))

(defun +lizqwer/straight-action (action prompt &optional is-yes)
  "Helper function to execute straight ACTIONS with PROMPT and optional PACKAGE."
  (let ((command (intern (format "straight-%s" action))))
    (if (or is-yes (yes-or-no-p (format "Run %s? " prompt)))
        (progn
          (call-interactively command)
          (message "run %s command finished" prompt))
      (message "cancel"))))

(defhydra hydra-straight-helper (:hint nil :color pink)
  "
Straight:
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all        |_o_pen package
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package    |_O_pen package web      ||_k_ prune build||
----------------^^+--------------^^+---------------^^+----------------^^+----------------^^+----------------------^^
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe entry|us_e_ package           ||_q_uit||
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |_G_et recipe repos|remov_E_ unused packages
"
  ("c" (+lizqwer/straight-action "check-all" "straight-check-all" t))
  ("C" (+lizqwer/straight-action "check-package" "straight-check-package" t))
  ("r" (+lizqwer/straight-action "rebuild-all" "straight-rebuild-all"))
  ("R" (+lizqwer/straight-action "rebuild-package" "straight-rebuild-package"))
  ("f" (+lizqwer/straight-action "fetch-all" "straight-fetch-all"))
  ("F" (+lizqwer/straight-action "fetch-package" "straight-fetch-package"))
  ("p" (+lizqwer/straight-action "pull-all" "straight-pull-all"))
  ("P" (+lizqwer/straight-action "pull-package" "straight-pull-package"))
  ("m" (+lizqwer/straight-action "merge-all" "straight-merge-all"))
  ("M" (+lizqwer/straight-action "merge-package" "straight-merge-package"))
  ("n" (+lizqwer/straight-action "normalize-all" "straight-normalize-all"))
  ("N" (+lizqwer/straight-action "normalize-package" "straight-normalize-package"))
  ("u" (+lizqwer/straight-action "push-all" "straight-push-all"))
  ("U" (+lizqwer/straight-action "push-package" "straight-push-package"))
  ("v" (+lizqwer/straight-action "freeze-versions" "straight-freeze-versions"))
  ("V" (+lizqwer/straight-action "thaw-versions" "straight-thaw-versions"))
  ("w" (+lizqwer/straight-action "watcher-start" "straight-watcher-start"))
  ("W" (+lizqwer/straight-action "watcher-quit" "straight-watcher-quit"))
  ("g" (+lizqwer/straight-action "get-recipe" "straight-get-recipe" t) :color blue)
  ("G" (+lizqwer/straight-action "pull-recipe-repositories" "straight-pull-recipe-repositories") :color blue)
  ("o" (+lizqwer/straight-action "visit-package" "straight-visit-package" t) :color blue)
  ("O" (+lizqwer/straight-action "visit-package-website" "straight-visit-package-website" t) :color blue)
  ("e" (+lizqwer/straight-action "use-package" "straight-use-package" t))
  ("E" (+lizqwer/straight-action "remove-unused-repos" "straight-remove-unused-repos") :color blue)
  ("k" (+lizqwer/straight-action "prune-build" "straight-prune-build") :color blue)
  ("q" nil)
  ("<escape>" nil :color blue))

(provide 'init-hydra)
;;; init-hydra.el ends here
