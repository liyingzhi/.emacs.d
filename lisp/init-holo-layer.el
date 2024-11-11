;;; init-holo-layer.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords:


(require 'holo-layer)
(setq holo-layer-enable-cursor-animation t)

(setq holo-layer-enable-window-border t)
(setq holo-layer-active-window-color "#ff0000")
(setq holo-layer-inactive-window-color "#00ff00")

(setq holo-layer-hide-mode-line t)
(setq holo-layer-enable-place-info t)

(holo-layer-enable)

(provide 'init-holo-layer)
