;;; init-theme.el --- theme                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path
             (locate-user-emacs-file "themes"))

;; kanagawa-* themes
(when (string-prefix-p "kanagawa-"
                       (symbol-name user/night-theme))
  (setq kanagawa-themes-org-height nil))

(+lizqwer/load-theme user/night-theme)

;; catppuccin themes
(when (eq user/night-theme 'catppuccin)
  (setq catppuccin-flavor user/catppuccin-flavor)
  (catppuccin-reload))

;; modus and derivative themes
(modus-themes-include-derivatives-mode 1)
(global-set-keys
 '(("<f7>" . modus-themes-rotate)
   ("C-<f7>" . modus-themes-select)
   ("M-<f7>" . modus-themes-load-random)))

(provide 'init-theme)
;;; init-theme.el ends here
