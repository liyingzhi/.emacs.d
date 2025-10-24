;;; init-yaml.el --- yaml                            -*- lexical-binding: t; -*-
;;; Commentary: ;;
;;; Code:

(wait-packages! '(yaml-pro))

(add-hook 'yaml-ts-mode-hook 'yaml-pro-ts-mode)

(with-eval-after-load 'yaml-ts-mode

  (keymap-sets yaml-ts-mode-map
    '(("C-c C-f" . outline-indent-close-fold)
      ("C-c C-o" . outline-indent-open-fold-rec)
      ("C-c C-t" . outline-indent-toggle-level-at-point)
      ("C-c TAB" . outline-cycle-buffer)
      ("<backtab>" . outline-cycle))))

(with-eval-after-load 'yaml-pro
  (defvar-keymap my/yaml-pro/tree-repeat-map
    :repeat t
    "n" #'yaml-pro-ts-next-subtree
    "p" #'yaml-pro-ts-prev-subtree
    "u" #'yaml-pro-ts-up-level
    "d" #'yaml-pro-ts-down-level
    "m" #'yaml-pro-ts-mark-subtree
    "k" #'yaml-pro-ts-kill-subtree
    "a" #'yaml-pro-ts-first-sibling
    "e" #'yaml-pro-ts-last-sibling
    ">" #'yaml-pro-ts-indent-subtree
    "<" #'yaml-pro-ts-unindent-subtree
    "SPC" #'my/yaml-pro/set-mark)

  (defun my/yaml-pro/set-mark ()
    "Toggle the mark in yaml-pro mode.
This command acts as a toggle for the mark:
- If mark is active, deactivate it
- If mark is not active, activate it
The toggle behavior only occurs when this command is repeated."
    (interactive)
    (my/region/set-mark 'my/yaml-pro/set-mark))

  (defun my/region/set-mark (command-name)
    "Set or toggle the mark region based on COMMAND-NAME.
If COMMAND-NAME equals last-command and mark is active, deactivate mark.
If COMMAND-NAME equals last-command and mark is not active, activate mark.
Otherwise, set mark normally using `set-mark-command'."
    (if (eq last-command command-name)
        (if (region-active-p)
            (progn
              (deactivate-mark)
              (message "Mark deactivated"))
          (activate-mark)
          (message "Mark activated"))
      (set-mark-command nil))))


(with-eval-after-load 'yaml-pro
  (keymap-unset yaml-pro-mode-map "C-c '")
  (keymap-unset yaml-pro-ts-mode-map "C-c '"))

(provide 'init-yaml)
;;; init-yaml.el ends here
