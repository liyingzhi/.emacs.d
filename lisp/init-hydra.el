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
   (("f" flycheck-mode "flycheck" :toggle t)
    ("v" global-diff-hl-mode "diff-hl mode" :toggle t)
    ("M" diff-hl-margin-mode "margin gutter" :toggle t)
    ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
    ("m" +lizqwer/toggle-move-style "move style" :toggle user/move-style-motion))
   "LLM"
   (("G" my/switch-gptel-llm "llm free" :toggle (equal gptel-model user/ai-model-free))
    ("C" my/switch-gptel-llm-coder "llm coder" :toggle (equal gptel-model user/ai-model-coder)))))

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
   (("s" (lambda ()
           (interactive)
           (autoload 'consult-fd-dir "init-func" nil t)
           (consult-fd-dir)) "Fuzzy search Dir")
    ("j" dired-jump "Dired jump")
    ("J" dired-jump-other-window "Dired jump other"))))

(defhydra hydra-straight-helper (:hint nil)
  "
Straight:
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all        |_o_pen package
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package    |_O_pen package web
----------------^^+--------------^^+---------------^^+----------------^^+----------------^^+------------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe entry|us_e_ package
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |_G_et recipe repos|remov_E_ unused packages
"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("G" straight-pull-recipe-repositories)
  ("o" straight-visit-package)
  ("O" straight-visit-package-website)
  ("e" straight-use-package)
  ("E" straight-remove-unused-repos)
  ("q" nil))

(provide 'init-hydra)
;;; init-hydra.el ends here
