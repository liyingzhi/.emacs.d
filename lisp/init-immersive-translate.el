;;; init-immersive-translate.el --- immersive        -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'immersive-translate)

(setq immersive-translate-backend 'trans)
(setq immersive-translate-trans-default-args
      (concat " " immersive-translate-trans-default-args))

(provide 'init-immersive-translate)
;;; init-immersive-translate.el ends here
