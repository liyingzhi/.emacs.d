;;; init-python.el --- init python package           -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwer@lzb>
;; Keywords: processes

;;; Commentary:


;;; Code:
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home
        (expand-file-name "/opt/anaconda"))
  (setq conda-env-home-directory
        (expand-file-name "~/.conda")))

(provide 'init-python)
;;; init-python.el ends here.