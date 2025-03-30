(defun +lizqwer/load-theme (new-theme)
  "Load theme."
  (unless (cl-find new-theme custom-enabled-themes)
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (ignore-errors
      (load-theme new-theme t nil))
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

(provide 'init-theme)
