;;; init-theme.el --- theme                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; kanagawa-* themes
(when (string-prefix-p "kanagawa-"
                       (symbol-name user/night-theme))
  (setq kanagawa-themes-org-height nil))

(when (eq user/night-theme 'modus-vivendi-catppuccin-mocha)
  (require 'modus-vivendi-catppuccin-mocha-theme)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  (setq modus-themes-prompts '(bold intense)))

(+lizqwer/load-theme user/night-theme)

;; catppuccin themes
(when (eq user/night-theme 'catppuccin)
  (setq catppuccin-flavor user/catppuccin-flavor)
  (catppuccin-reload))

(provide 'init-theme)
;;; init-theme.el ends here
