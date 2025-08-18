;;; init-eaf.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp,
(setq eaf-dired-advisor-enable nil)
(setq eaf-python-command user/eaf-python-command)
;; (setq eaf-browser-auto-import-chrome-cookies t)
(setq eaf-pyqterminal-font-family "Hack Nerd Font")
(setq eaf-config-location no-littering-var-directory)

(require 'eaf)
(unless eaf-dired-advisor-enable
  (advice-remove #'find-file  #'eaf--find-file-advisor)
  (advice-remove #'org-open-file  #'eaf--find-file-advisor))

(require 'eaf-browser)
(require 'eaf-org-previewer)
(require 'eaf-music-player)
(require 'eaf-video-player)
(require 'eaf-pdf-viewer)
(require 'eaf-browser)
(require 'eaf-pyqterminal)
(require 'eaf-markdown-previewer)
(require 'eaf-map)
(require 'eaf-git)
(require 'eaf-mindmap)
(require 'eaf-image-viewer)
(require 'eaf-jupyter)
(require 'eaf-system-monitor)
(when user/load-eaf
  (require 'eaf-file-manager))
(require 'eaf-markmap)
(require 'eaf-mind-elixir)
;; (require 'eaf-file-browser)
;; (require 'eaf-terminal)
;; (require 'eaf-airshare)
;; (require 'eaf-file-sender)
;; (require 'eaf-js-video-plyer)
;; (require 'eaf-rss-reader)

;; (eaf-bind-key meow-keypad "SPC" eaf-git-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-mindmap-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-pdf-viewer-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-image-viewer-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-music-player-keybinding)
;; (eaf-bind-key js_toggle_play_status "M-t" eaf-music-player-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-video-player-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-org-previewer-keybinding)

;; (eaf-bind-key meow-keypad "SPC" eaf-browser-caret-mode-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-markdown-previewer-keybinding)
;; (eaf-bind-key meow-keypad "SPC" eaf-pyqterminal-cursor-move-mode-keybinding)

(keymap-sets eaf-mode-map*
  '(("C-c b" . one-key-menu-buffer)
    ("C-k" . kill-now-buffer)
    ("C-q" . my/meow-quit)
    ("C-c <SPC>" . meow-keypad)))

(provide 'init-eaf)
