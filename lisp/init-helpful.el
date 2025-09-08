;;; init-helpful.el --- helpful                     -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(lazy-load-global-keys
 '(("C-h f" . helpful-callable)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)
   ("C-h F" . helpful-function)
   ("C-h C" . help-command)) "helpful")

(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

(require 'helpful)
(add-hooks '(help-mode helpful-mode)
           #'visual-line-mode)
(keymap-sets (help-mode-map helpful-mode-map)
  '(("M-<left>" . previous-buffer-dedicated-window)
    ("M-<right>" . next-buffer-dedicated-window)))

(provide 'init-helpful)
;;; init-helpful.el ends here
