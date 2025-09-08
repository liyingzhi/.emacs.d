;;; init-c++.el --- c++                              -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;config c++ style
(setq c-default-style "linux"
      c-basic-offset 4
      c-ts-mode-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\CMakeLists.txt\\'" . cmake-ts-mode))

(add-hook 'c-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (c-toggle-auto-hungry-state)))

(with-eval-after-load 'c++-ts-mode
  (defun run-or-compile-c++ ()
    "Run or compile c++ project."
    (interactive)
    (require 'init-project)
    (call-interactively #'projection-commands-build-project))

  (keymap-sets (c++-mode-map c++-ts-mode-map)
    '(("C-c r" . run-or-compile-c++))))

(provide 'init-c++)
;;; init-c++.el ends here
