;;; init-spell.el --- spell                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wucuo)

;;; flyspell
(set-face-attribute 'flyspell-incorrect nil
                    :underline '(:style line :color "Pink")
                    :foreground nil
                    :background nil)

(set-face-attribute 'flyspell-duplicate nil
                    :underline '(:style line :color "DeepPink")
                    :foreground nil
                    :background nil)
(keymap-global-set "C-M-$" #'ispell-word)

;; (setq wucuo-font-faces-to-check '(font-lock-comment-face))
;; (setq wucuo-flyspell-start-mode "normal")

(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))

;;; wucuo
(setq wucuo-spell-check-buffer-predicate
      (lambda ()
        (not (memq major-mode
                 '(dired-mode
                   log-edit-mode
                   compilation-mode
                   help-mode
                   profiler-report-mode
                   speedbar-mode
                   gud-mode
                   calc-mode
                   Info-mode)))))

(add-hooks '(prog-mode text-mode)
           #'(lambda ()
               (wucuo-start)))

(keymap-global-set "C-M-$" #'ispell-word)

(provide 'init-spell)
;;; init-spell.el ends here.
