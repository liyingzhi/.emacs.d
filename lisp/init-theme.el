;;; init-theme.el --- theme                          -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path
             (locate-user-emacs-file "themes"))

(defcustom user/day-theme 'modus-operandi-tinted
  "Day theme name."
  :group 'user
  :type 'symbol)

(defcustom user/night-theme 'doom-dracula
  "Night theme name."
  :group 'user
  :type 'symbol)

(require 'consult)

(defun customize-theme (theme &optional previewp)
  "Customize THEME.
PREVIEWP is preview the theme without enabling it permanently."
  (interactive (list (let* ((regexp (consult--regexp-filter
                                     (mapcar (lambda (x) (if (stringp x) x (format "\\`%s\\'" x)))
                                             consult-themes)))
                            (avail-themes (seq-filter
                                           (lambda (x) (string-match-p regexp (symbol-name x)))
                                           (cons 'default (custom-available-themes))))
                            (saved-theme (car custom-enabled-themes)))
                       (consult--read
                        (mapcar #'symbol-name avail-themes)
                        :prompt "Theme: "
                        :require-match t
                        :category 'theme
                        :history 'consult--theme-history
                        :lookup (lambda (selected &rest _)
                                  (setq selected (and selected (intern-soft selected)))
                                  (or (and selected (car (memq selected avail-themes)))
                                      saved-theme))
                        :state (lambda (action theme)
                                 (with-selected-window (or (active-minibuffer-window)
                                                           (selected-window))
                                   (pcase action
                                     ('return (customize-theme (or theme saved-theme) t))
                                     ((and 'preview (guard theme)) (customize-theme theme t)))))
                        :default (symbol-name (or saved-theme 'default))))))
  (when (eq theme 'default) (setq theme nil))
  (when theme
    (if previewp
        (unless (eq theme (car custom-enabled-themes))
          (mapc #'disable-theme custom-enabled-themes)
          (unless (and (memq theme custom-known-themes) (get theme 'theme-settings))
            (load-theme theme 'no-confirm 'no-enable))
          (if (and (memq theme custom-known-themes) (get theme 'theme-settings))
              (enable-theme theme)
            (consult--minibuffer-message "%s is not a valid theme" theme)))
      (when-let* ((table '(("Night" . user/night-theme)
                           ("Day" . user/day-theme)))
                  (comp (consult--read table
                                       :prompt "Theme Category: "
                                       :require-match t))
                  (symbol (alist-get comp table nil nil #'equal)))
        (+lizqwer/load-theme theme)
        (customize-save-variable symbol theme)))))

;; kanagawa-* themes
(when (string-prefix-p "kanagawa-"
                       (symbol-name user/night-theme))
  (setq kanagawa-themes-org-height nil))

;; Reference: https://taxodium.ink/emacs-random-theme.html
(defun spike-leung/load-theme-by-time (light-fn dark-fn)
  "Call LIGHT-FN  to load light themes from 8:00a.m to 6:00p.m.
otherwise, call DARK-FN to load dark themes."
  (let ((hour (string-to-number (format-time-string "%H"))))
    (funcall (if (and (>= hour 8) (< hour 18)) light-fn dark-fn))))

(defun spike-leung/themes-load-random (&optional background-mode)
  "Random load themes.

Use `ef-themes' for Monday,Tuesday,Wednesday.
Use `modus-themes' on Thursday, Friday.
Use `doric-themes' on Saturday, Sunday.
Use light themes at day, use dark themes at night.

With optional BACKGROUND-MODE as a prefix argument, prompt to limit the
set of themes to either dark or light variants."
  (require 'modus-themes)
  (require 'ef-themes)
  (require 'doric-themes)
  (let ((week (string-to-number (format-time-string "%u"))))
    (cond
     ((<= week 3)
      (cond
       ((eq background-mode 'light) (ef-themes-load-random-light))
       ((eq background-mode 'dark) (ef-themes-load-random-dark))
       (t (spike-leung/load-theme-by-time #'ef-themes-load-random-light #'ef-themes-load-random-dark))))
     ((<= week 5)
      (if background-mode
          (modus-themes-load-random background-mode)
        (spike-leung/load-theme-by-time
         (lambda () (modus-themes-load-random 'light))
         (lambda () (modus-themes-load-random 'dark)))))
     (t

      (if background-mode
          (doric-themes-load-random background-mode)
        (spike-leung/load-theme-by-time
         (lambda () (doric-themes-load-random 'light))
         (lambda () (doric-themes-load-random 'dark))))))))

(defun random-theme-light()
  "Load random light themes."
  (interactive)
  (spike-leung/themes-load-random 'light))

(defun random-theme-dark()
  "Load random dark themes."
  (interactive)
  (spike-leung/themes-load-random 'dark))

(if user/auto-random-theme
    (spike-leung/themes-load-random)

  (+lizqwer/load-theme user/night-theme)
  ;; catppuccin themes
  (when (eq user/night-theme 'catppuccin)
    (setq catppuccin-flavor user/catppuccin-flavor)
    (catppuccin-reload)))

;; modus and derivative themes
(modus-themes-include-derivatives-mode 1)
(global-set-keys
 '(("<f7>" . modus-themes-rotate)
   ("C-<f7>" . modus-themes-select)
   ("M-<f7>" . modus-themes-load-random)))

(provide 'init-theme)
;;; init-theme.el ends here
