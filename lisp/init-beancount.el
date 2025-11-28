;;; init-beancount.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  fly_lilee

;; Author: fly_lilee <liyl2015@mail.ustc.edu.cn>
;; Keywords: tools


(wait-packages! '((beancount-mode :host github :repo "beancount/beancount-mode")))

(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.\\(beancount\\|bean\\)\\'" . beancount-mode))

(add-hook 'beancount-mode-hook
          (lambda ()
            (setq-local electric-indent-chars nil)))

(add-hook 'beancount-mode-hook #'outline-minor-mode)

(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-u") #'outline-up-heading)

(with-eval-after-load 'init-program
  (pcase user/diagnostic
    ('flymake
     (add-hook 'beancount-mode-hook #'flymake-bean-check-enable))))

(defvar beancount-journal-command
  (concat
   "select date, flag, maxwidth(description, 80), position, balance "
   "where account = '%s'"))

(defun beancount-query-journal-at-point ()
  "Run a journal command for the account at point."
  (interactive)
  (let ((account (thing-at-point 'beancount-account)))
    (beancount--run beancount-query-program
                    (file-relative-name buffer-file-name)
                    (format beancount-journal-command account))))

(define-key beancount-mode-map (kbd "C-j") #'beancount-query-journal-at-point)


;; TODO: Refine this a bit later on.
(defvar beancount-balance-command
  (concat
   "select account, sum(position) "
   "where account ~ '%s' "
   "group by 1 "
   "order by 1"))

(defun beancount-query-balance-at-point ()
  "Run a balance command for the account at point."
  (interactive)
  (let ((account (thing-at-point 'beancount-account)))
    (beancount--run beancount-query-program
                    (file-relative-name buffer-file-name)
                    (format beancount-balance-command account))))

(define-key beancount-mode-map (kbd "C-S-j") #'beancount-query-balance-at-point)

(provide 'init-beancount)
