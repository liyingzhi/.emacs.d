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
(keymap-sets helpful-mode-map
  '(("M-n" . scroll-up-1/3)
    ("M-p" . scroll-down-1/3)))

(provide 'init-helpful)
