;;; init-ui.el --- init ui packages                  -*- lexical-binding: t; -*-

;; Copyright (C) 2022

;; Author:  <lizqwerscott@gmail.com>

;;; Commentary:

;;; Code:


;;; Optimization
(setq idle-update-delay 1.0)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

;;; Title
(setq frame-title-format '("Emacs - %b")
      icon-title-format frame-title-format)

(setq default-frame-alist
      '((alpha-background . 50)
        ;; (fullscreen . maximized)
        ))

(setq initial-frame-alist
      '((top . 0.5)
        (left . 0.5)
        (width . 0.9)
        (height . 0.9)
        ;; (fullscreen . maximized)
        ))
(set-frame-parameter nil 'alpha user/init-alpha-background)

(defun my/toggle-frame-transparency ()
  "Toggle frame transparency with user-specified opacity value.
Prompts user whether to enable transparency. If yes, asks for opacity value (0-100).
If no, restores full opacity. Only affects the active frame."
  (interactive)
  (if (y-or-n-p "Enable frame transparency? ")
      (let ((alpha-value (read-number "Enter transparency value (0-100, default 90): " 90)))
        (if (and (>= alpha-value 0) (<= alpha-value 100))
            (progn
              (set-frame-parameter nil 'alpha alpha-value)
              (message "Frame transparency set to %d%%" alpha-value))
          (message "Invalid transparency value. Please enter a number between 0 and 100.")))
    (progn
      (set-frame-parameter nil 'alpha 100)
      (message "Frame transparency disabled (full opacity restored)"))))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (toggle-frame-maximized)
;; (toggle-frame-fullscreen)

;;; Head line
;; Show the current function name in the header line
;; (which-function-mode)
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))
;; (setq mode-line-misc-info
;;             ;; We remove Which Function Mode from the mode line, because it's mostly
;;             ;; invisible here anyway.
;;             (assq-delete-all 'which-func-mode mode-line-misc-info))

;; (add-hook 'after-init-hook
;;           #'(lambda ()
;;               (if (get-buffer "*Netease-Cloud-Music*")
;;                   (netease-cloud-music-add-header-lyrics))))

(custom-set-faces
 '(header-line ((t (:inherit t :foreground unspecified :background unspecified)))))

(breadcrumb-mode)
;; (setq header-line-format nil)

;;; Line number
(add-hooks '(prog-mode text-mode conf-mode)
             #'(lambda ()
                 (setq display-line-numbers-type 'relative)
                 (display-line-numbers-mode 1)))

;;; Suppress GUI features
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen nil
      inhibit-startup-message t)

;;; Mouse & Smooth Scroll
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
        mouse-wheel-scroll-amount-horizontal 1
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      auto-window-vscroll nil
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

;; 平滑地进行半屏滚动，避免滚动后recenter操作
(if (fboundp 'pixel-scroll-precision-mode)
    (pixel-scroll-precision-mode t)
  (pixel-scroll-mode t))

;;; Logo
(setq fancy-splash-image user/logo)

(require 'init-dashboard)
(require 'init-posframe)
(require 'init-highlight)
(require 'init-window)

;; (require 'init-holo-layer)

;;; Buffer Name
(require 'buffer-name-relative)
(setq buffer-name-relative-prefix '("<" . "> ")
      buffer-name-relative-fallback 'default)


(defun buffer-name-relative-root-path-from-project (filepath)
  "Return the PROJECT directory from FILEPATH or nil."
  (let ((result nil))
    (when (fboundp 'project-root)
      (let ((dir (if (file-directory-p filepath)
                     (directory-file-name filepath)
                   (file-name-directory filepath))))
        (when (and dir (project-current nil dir))
          (condition-case-unless-debug err
              (setq result (project-root (project-current nil dir)))
            (error (message "Error finding PROJECT root name: %s" err))))))
    result))
(setq buffer-name-relative-root-functions '(buffer-name-relative-root-path-from-project))
(add-hook 'after-init-hook #'buffer-name-relative-mode)


;;; Another

;; (require 'zone)
;; (zone-when-idle 600)

(global-so-long-mode 1)

(global-hl-line-mode 1)
(add-hook 'emacs-lisp-mode-hook 'outshine-mode)


;;; Ibuffer
(add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
(add-hook 'ibuffer-mode-hook
          #'(lambda ()
              (unless ibuffer-filter-groups
                (require 'projection-ibuffer)
                (ibuffer-projection-set-filter-groups))))

(require 'init-image-slicing)

(provide 'init-ui)
;;; init-ui.el ends here.
