;;; init-org-presentation.el --- org presentations stack  -*- lexical-binding: t; -*-
;;; Commentary:
;;; refs: https://ankit.earth/blog/my-emacs-presentation-stack/
;;; Code:

;;; olivetti
(with-eval-after-load 'olivetti
  ;; Fancy mode uses margins and fringes to create vertical
  ;; blocks on either sides of the content.
  (setopt olivetti-style 'fancy
          olivetti-body-width 0.5
          olivetti-margin-width 5
          olivetti-minimum-body-width 80
          olivetti-recall-visual-line-mode-entry-state t)

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
			  (setq-local indicate-buffer-boundaries 'left))))

;;; logos
(with-eval-after-load 'logos
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
  (define-key org-mode-map [remap backward-page] 'logos-backward-page-dwim))

;;; presentation with logos utils
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

;;; dslide
(with-eval-after-load 'dslide
  (setopt dslide-slide-in-blank-lines 2
          dslide-breadcrumb-separator " » "
          dslide-breadcrumb-separator-style 'append)
  (remove-hook 'dslide-start-hook #'dslide-cursor-hide)
  (remove-hook 'dslide-present-hook #'dslide-cursor-hide)
  (remove-hook 'dslide-develop-hook #'dslide-cursor-hide)
  (with-hook dslide-start-hook
    (org-indent-mode -1))

  (keymap-sets dslide-mode-map
    '(("<down>" . nil)
      ("C-<down>" . dslide-deck-stop))))

;;;###autoload
(defun my/dslide-deck-start-dwim (&optional arg)
  "Start a dslide deck in the current Org buffer.
Without a prefix argument call `dslide-deck-start'.  With a prefix argument
ARG call `dslide-deck-present', which starts the deck in a new
presentation frame."
  (interactive "P")
  (if arg
      (dslide-deck-present)
    (dslide-deck-start)))

(keymap-sets org-mode-map
  '(("<f5>" . my/dslide-deck-start-dwim)))

(provide 'init-org-presentation)
;;; init-org-presentation.el ends here
