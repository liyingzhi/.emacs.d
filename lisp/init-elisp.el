;;; init-elisp.el --- elisp                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(keymap-set emacs-lisp-mode-map
            "C-c C-p"
            #'ielm)

(keymap-set lisp-interaction-mode-map
            "C-c C-p"
            #'ielm)

(add-hook 'lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))


(add-hook 'emacs-lisp-mode-hook 'outshine-mode)
(add-hook 'emacs-lisp-mode-hook 'eros-mode)

(provide 'init-elisp)
;;; init-elisp.el ends here
