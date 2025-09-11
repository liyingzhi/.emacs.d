;;; init-theme.el --- theme                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; 随机选择列表中的一个元素
(defun a-random (list)
  "Randomly select an element from LIST."
  (nth (random (length list)) list))

;; 随机加载一个白名单主题
(defun a-random-theme (&optional use-light)
  "Randomly load a theme from the whitelist.
When called interactively, press t to select a light theme.
The USE-LIGHT argument specifies whether to use light themes."
  (interactive
   (list (eq (read-char "Press `t' for light theme:") ?t)))
  (let ((theme-pool (remove (car custom-enabled-themes)
                            (if use-light
                                a-theme-whitelist-light
                              a-theme-whitelist-dark))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (a-random theme-pool) t)
    (unless user/show-modeline
      ;; Disable mode line.
      (set-face-attribute 'mode-line nil
                          :foreground "DarkRed"
                          :background "DarkRed"
                          :height 0.1
                          :box nil)
      (set-face-attribute 'mode-line-inactive nil
                          :foreground "Gay10"
                          :background "Gay10"
                          :height 0.1
                          :box nil
                          :inherit 'unspecified))))

(+lizqwer/load-theme user/night-theme)

(when (eq user/night-theme 'catppuccin)
  (setq catppuccin-flavor user/catppuccin-flavor)
  (catppuccin-reload))

(provide 'init-theme)
;;; init-theme.el ends here
