(add-hook 'typescript-tsx-mode-hook
          #'apheleia-mode)

(add-hook 'json-mode-hook
          #'apheleia-mode)

(add-hook 'jsonian-mode-hook
          #'apheleia-mode)

(add-hook 'sly-mode-hook
          #'(lambda ()
              (unless (sly-connected-p)
                (save-excursion (sly)))))

(add-hook 'emacs-lisp-mode-hook 'outshine-mode)


;; Enable `read-only-mode' to ensure that we don't change what we can't read.
(add-hook 'redacted-mode-hook
          (lambda ()
            (read-only-mode
             (if redacted-mode 1 -1))))

;;; macrostep-mode-map settings
(add-hook 'macrostep-mode-hook
          #'(lambda ()
              (with-eval-after-load 'meow
                (keymap-sets macrostep-mode-map
                  '(("j" . meow-next)
                    ("k" . meow-prev)
                    ("h" . meow-left)
                    ("l" . meow-right))))))

(provide 'init-hook)
