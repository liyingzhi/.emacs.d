;;; init-pangu.el --- spacing between chinese and asicc char  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-hooks '(markdown-mode org-mode)
           #'pangu-spacing-mode)

(setq pangu-spacing-real-insert-separtor t)

(provide 'init-pangu)
;;; init-pangu.el ends here
