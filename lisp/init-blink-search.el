;;; init-blink-search.el --- Configure blink-search package  -*- lexical-binding: t; -*-

;; Package-Requires: ((blink-search))

;;; Commentary:
;; Configuration for blink-search, a modern multiple source search UI for Emacs.

;;; Code:

(setq blink-search-history-path (expand-file-name
                                 (concat no-littering-var-directory
                                         (file-name-as-directory "blink-search")
                                         "history.txt")))
(setq blink-search-db-path (expand-file-name "blink-search.db" no-littering-var-directory))

(require 'blink-search)
;; (add-hook 'blink-search-mode-hook #'meow-insert-mode)
;; (setq blink-search-search-backends
;;       '("Buffer List" "Find File" "Common Directory" "Recent File" "Grep PDF" "PDF" "Google Suggest"))
(setq blink-search-search-backends
      '("Current Buffer" "Buffer List" "Find File" "Grep File" "Common Directory" "Recent File" "Google Suggest"))

(setq blink-search-common-directory
      '(("HOME"  "~/")
        ("Project"  "~/MyProject/")
        ("Config"  "~/.emacs.d/")))

(keymap-set blink-search-mode-map
            "C-c C-k"
            #'blink-search-quit)

(setq blink-search-python-command user/run-python-command)
(setq blink-search-grep-pdf-search-paths "~/tankData/pdf")

(provide 'init-blink-search)
;;; init-blink-search.el ends here
