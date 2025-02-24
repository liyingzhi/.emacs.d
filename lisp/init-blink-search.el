(setq blink-search-history-path no-littering-var-directory)
(setq blink-search-db-path no-littering-var-directory)
(require 'blink-search)
;; (add-hook 'blink-search-mode-hook #'meow-insert-mode)
;; (setq blink-search-search-backends
;;       '("Buffer List" "Find File" "Common Directory" "Recent File" "Grep PDF" "PDF" "Google Suggest"))
(setq blink-search-search-backends
      '("Buffer List" "Find File" "Common Directory" "Recent File" "Google Suggest"))

(setq blink-search-common-directory
      '(("HOME"  "~/")
        ("Project"  "~/MyProject/")
        ("Config"  "~/.emacs.d/")))

(setq blink-search-python-command user/run-python-command)
(setq blink-search-grep-pdf-search-paths "~/tankData/pdf")

(provide 'init-blink-search)
