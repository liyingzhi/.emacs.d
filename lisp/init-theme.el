;;; init-theme.el --- theme                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(+lizqwer/load-theme user/night-theme)

(when (eq user/night-theme 'catppuccin)
  (setq catppuccin-flavor user/catppuccin-flavor)
  (catppuccin-reload))

(provide 'init-theme)
;;; init-theme.el ends here
