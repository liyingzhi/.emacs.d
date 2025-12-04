;;; init-org-presentation.el --- org presentations stack  -*- lexical-binding: t; -*-
;;; Commentary:
;;; refs: https://ankit.earth/blog/my-emacs-presentation-stack/
;;; Code:

;;; olivetti
(require 'olivetti)
;; Fancy mode uses margins and fringes to create vertical
;; blocks on either sides of the content.
(setq olivetti-style 'fancy)
(setq-default olivetti-body-width 0.5)
(add-hook 'olivetti-mode-on-hook
		  (lambda ()
			;; Disable Line numbers.
			;; (display-line-numbers-mode -1)
			;; Disable Buffer boundaries.
			(setq-local indicate-buffer-boundaries nil)))
(add-hook 'olivetti-mode-off-hook
		  (lambda ()
			;; Enable Line numbers.
			;; (display-line-numbers-mode 1)
			;; Restores Buffer boundaries.
			(setq-local indicate-buffer-boundaries 'left)))

;;; logos
(require 'logos)
(defun arg-reveal-entry ()
  "Reveal Org or Outline entry."
  (cond
   ((and (eq major-mode 'org-mode)
         (org-at-heading-p))
    (org-show-entry))
   ((or (eq major-mode 'outline-mode)
        (bound-and-true-p outline-minor-mode))
    (outline-show-entry))))

(setq-default logos-olivetti t
              logos-outlines-are-pages t
              logos-hide-mode-line t
              logos-hide-header-line t)

(with-hook logos-page-motion-hook
  (arg-reveal-entry))

(define-key org-mode-map [remap forward-page] 'logos-forward-page-dwim)
(define-key org-mode-map [remap backward-page] 'logos-backward-page-dwim)


;;; utils

(defvar-local arg-presentation nil
  "Non-nil when this buffer is in presentation mode.")

(defun arg-start-presentation ()
  "Start Presentation - Configures Frame, Style, etc."
  (interactive)
  (when (eq major-mode 'org-mode)
    (setq-local arg-presentation t)
    (logos-focus-mode 1)
    (logos-narrow-dwim)
    (org-fold-show-entry)))

(defun arg-stop-presentation ()
  "Stop Presentation - Reverts Frame, Style, etc."
  (interactive)
  (cond ((not (eq major-mode 'org-mode))
         (error "This command only works in Org buffers."))
        ((not arg-presentation)
         (error "Not presenting right now."))
        (t (progn
             (logos-focus-mode -1)
             (widen)
             (setq-local arg-presentation nil)))))

(defun arg-toggle-presentation ()
  "Toggle(start or stop) Presentation."
  (interactive)
  (if (eq major-mode 'org-mode)
      (if arg-presentation
          (arg-stop-presentation)
        (arg-start-presentation))
    (error "This command only works in Org buffers.")))


(provide 'init-org-presentation)
;;; init-org-presentation.el ends here
