;;; init-spell.el --- spell                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'wucuo)

;;; ispell
(setq ispell-program-name "aspell")
;; You could add extra option "--camel-case" for camel case code spell checking if Aspell 0.60.8+ is installed
;; @see https://github.com/redguardtoo/emacs.d/issues/796
(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together" "--run-together-limit=16"))
(setq ispell-silently-savep t)

;;; flyspell
(set-face-attribute 'flyspell-incorrect nil
                    :underline '(:style line :color "Pink")
                    :foreground 'unspecified
                    :background 'unspecified)

(set-face-attribute 'flyspell-duplicate nil
                    :underline '(:style line :color "DeepPink")
                    :foreground 'unspecified
                    :background 'unspecified)

;;; flyspell-correct
(with-eval-after-load 'flyspell
  (require 'flyspell-correct)
  (keymap-sets flyspell-mode-map
    `((("C-," "s-,") . ,(lambda ()
                          (interactive)
                          (flyspell-goto-next-error t)))
      (("C-." "s-.") . flyspell-goto-next-error)
      ("C-M-," . flyspell-correct-previous)
      ("C-M-." . flyspell-correct-next)
      ("C-M-$" . flyspell-correct-at-point))))

;;; wucuo

;; (setq wucuo-font-faces-to-check '(font-lock-comment-face))
;; (setq wucuo-flyspell-start-mode "normal")

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

(provide 'init-spell)
;;; init-spell.el ends here.
