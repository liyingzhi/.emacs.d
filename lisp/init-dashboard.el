;;; init-dashboard.el --- init emacs dashboard       -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'nerd-icons)
(require 'weather)

(defvar *start-banner* (propertize ";;     *
;;      May the Code be with You!
;;     .                                 .
;;                               *
;;          /\\/|_      __/\\\\
;;         /    -\\    /-   ~\\  .              \\='
;;         \\    = Y =T_ =   /
;;          )==*(\\=`     \\=`) ~ \\
;;         /     \\     /     \\
;;         |     |     ) ~   (
;;        /       \\   /     ~ \\
;;        \\       /   \\~     ~/
;; _/\\_/\\_/\\__  _/_/\\_/\\__~__/_/\\_/\\_/\\_/\\_/\\_
;; |  |  |  | ) ) |  |  | ((  |  |  |  |  |  |
;; |  |  |  |( (  |  |  |  \\\\ |  |  |  |  |  |
;; |  |  |  | )_) |  |  |  |))|  |  |  |  |  |
;; |  |  |  |  |  |  |  |  (/ |  |  |  |  |  |
;; |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
\n" 'face '(:foreground "green")))

(defvar *start-image-banner*
  (find-image
   `(( :type png
       :file ,(expand-file-name "logo_black_medium.png" user-emacs-directory))))
  "Banner images.")

(defvar dashboard-recover-layout-p nil
  "Whether recovers the layout.")

;; 自定义 *scratch* 内容
;;;###autoload
(defun +evan/scratch-setup()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer "*scratch*")
      ;; (erase-buffer)
      (insert *start-banner*)
      ;; (insert-image *start-image-banner* "Emacs")
      (insert "\n")
      (insert (format "启动时长: %s" (emacs-init-time)))
      (insert "\n")
      (insert-button "Quit Emacs"
                     'action (lambda (_button)
                               (save-buffers-kill-emacs)))
      (insert "\n")
      ;; (insert "Recent Files\n")
      ;; (dolist (f recentf-list)
      ;;   (insert-button f
      ;;                  'action (lambda (region)
      ;;                         (require 'f)
      ;;                         (let* ((f (buffer-substring-no-properties (overlay-start region) (overlay-end region)))
      ;;                                (fname (f-filename f)))
      ;;                           (find-file-noselect f)
      ;;                           (switch-to-buffer fname))))
      ;;   (insert "\n"))
      ))
  (goto-char (point-max)))

(add-hook 'dashboard-mode-hook
          #'page-break-lines-mode)

(with-eval-after-load 'page-break-lines
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

(custom-set-faces
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(dashboard-items-face ((t (:weight normal))))
 '(dashboard-no-items-face ((t (:weight normal)))))

(setq dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing"
      dashboard-startup-banner (or user/logo 'official)
      dashboard-image-banner-max-width 300
      dashboard-image-banner-max-height 150
      dashboard-path-max-length 60
      dashboard-path-style 'truncate-middle
      dashboard-page-separator "\f\n"
      dashboard-center-content t
      dashboard-vertically-center-content t
      dashboard-projects-backend 'project-el

      dashboard-items '((recents . 5)
                        ;; (bookmarks . 5)
                        (projects . 5)
                        ;; (agenda . 5)
                        )
      dashboard-startupify-list '(dashboard-insert-banner
                                  dashboard-insert-newline
                                  dashboard-insert-banner-title
                                  dashboard-insert-newline
                                  my-dashboard-insert-weather-info
                                  dashboard-insert-newline
                                  my-dashboard-insert-time
                                  ;; dashboard-insert-newline
                                  ;; dashboard-insert-navigator
                                  dashboard-insert-newline
                                  dashboard-insert-init-info
                                  dashboard-insert-items
                                  ;; dashboard-insert-newline
                                  dashboard-insert-footer)

      dashboard-display-icons-p t
      dashboard-icon-type 'nerd-icons
      dashboard-set-heading-icons t
      dashboard-set-file-icons t
      dashboard-heading-icons '((recents   . "nf-oct-history")
                                (bookmarks . "nf-oct-bookmark")
                                (agenda    . "nf-oct-calendar")
                                (projects  . "nf-oct-briefcase")
                                (registers . "nf-oct-database"))
      dashboard-footer-icon
      (nerd-icons-octicon "nf-oct-heart" :height 1.2 :face 'nerd-icons-lred)

      dashboard-set-init-info t
      dashboard-set-footer t)

(with-eval-after-load 'dashboard
  (defun my-dashboard-insert-copyright ()
    "Insert copyright in the footer."
    (dashboard-insert-center
     (propertize (format "\nPowered by Lizqwer Scott, %s\n" (format-time-string "%Y"))
                 'face 'font-lock-comment-face)))
  (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright)

  ;; (defun my-dashboard-insert-time ()
  ;;   "Insert time info."
  ;;   (dashboard-insert-center
  ;;    (propertize (format "%s\n" (format-time-string "%A, %B %d %R")) 'face 'font-lock-comment-face)))

  (defun my-dashboard-insert-time ()
    "Insert the current time and Chinese lunar date into the dashboard.
The format includes the weekday, Gregorian date, Chinese lunar month and day,
and the current time. The lunar date is displayed with a smaller font and
normal weight to distinguish it from other elements."
    (let* ((cn-date (calendar-chinese-from-absolute
                     (calendar-absolute-from-gregorian (calendar-current-date))))
           (cn-month (cl-caddr  cn-date))
           (cn-day   (cl-cadddr cn-date))
           (lunar-cn-month-day (format "%s%s%s, "
                                       (aref cal-china-x-month-name (1-  (floor cn-month)))
                                       (if (integerp cn-month) "" "(闰月)")
                                       (aref cal-china-x-day-name (1- cn-day)))))
      (dashboard-insert-center
       (format "%s%s%s\n"
               (propertize (format-time-string "%A, %B %d, ")
                           'face 'font-lock-comment-face)
               (propertize lunar-cn-month-day
                           'face '(:inherit font-lock-comment-face
                                            :height 0.9
                                            :weight normal))
               (propertize (format-time-string "%R")
                           'face 'warning)))))

  (defun my-dashboard-insert-weather-info ()
    "Insert weather info."
    (dashboard-insert-center
     (weather-info))))

(defun open-dashboard ()
  "Open the *dashboard* buffer and jump to the first widget."
  (interactive)
  ;; Check if need to recover layout
  (if (length> (window-list-1)
               ;; exclude `treemacs' window
               (if (and (fboundp 'treemacs-current-visibility)
                        (eq (treemacs-current-visibility) 'visible))
                   2
                 1))
      (setq dashboard-recover-layout-p t))

  ;; Display dashboard in maximized window
  (delete-other-windows)

  (let ((buf (get-buffer dashboard-buffer-name)))
    (unless buf
      (dashboard-open)))
  (weather-fetch-weather-data nil #'dashboard-refresh-buffer dashboard-buffer-name)

  (unless (weather--roi-window-is-active dashboard-buffer-name)
    (dashboard-refresh-buffer)))

(defun quit-dashboard ()
  "Quit dashboard window."
  (interactive)
  (quit-window t)

  ;; Recover layout
  (and dashboard-recover-layout-p
       (and (bound-and-true-p winner-mode) (winner-undo))
       (setq dashboard-recover-layout-p nil)))

;;; menu
(pretty-hydra-define hydra-dashboard
  (:title (pretty-hydra-title "Dashboard" 'mdicon "nf-md-view_dashboard")
          :color pink :quit-key ("q" "C-g"))
  ("Navigator"
   (("P" hydra-straight-helper/body "package manage" :exit t)
    ("S" find-custom-file "settings" :exit t)
    ("I" find-init-file "init file" :exit t))
   "Item"
   (("RET" widget-button-press "open" :exit t)
    ("<tab>" widget-forward "next")
    ("C-i" widget-forward "next")
    ("<backtab>" widget-backward "previous")
    ("C-n" next-line "next line")
    ("C-p" previous-line "previous  line"))
   "Misc"
   (("<f2>" open-dashboard "open" :exit t)
    ("g" dashboard-refresh-buffer "refresh" :exit t)
    ("Q" quit-dashboard "quit" :exit t))))

(global-set-keys
 '(("<f2>" . open-dashboard)))

(with-eval-after-load 'dashboard
  (keymap-sets dashboard-mode-map
    '(("S" . find-custom-file)
      ("I" . find-init-file)
      ("P" . hydra-straight-helper/body)
      ("q" . quit-dashboard)
      ("h" . hydra-dashboard/body)
      ("?" . hydra-dashboard/body))))

(defun dashboard-goto-recent-files ()
  "Go to recent files."
  (interactive)
  (let ((func (local-key-binding "r")))
    (and func (funcall func))))

(defun dashboard-goto-projects ()
  "Go to projects."
  (interactive)
  (let ((func (local-key-binding "p")))
    (and func (funcall func))))

(defun dashboard-goto-bookmarks ()
  "Go to bookmarks."
  (interactive)
  (let ((func (local-key-binding "m")))
    (and func (funcall func))))

(advice-add #'dashboard-refresh-buffer :after #'dashboard-jump-to-recents)

(pcase user/dashboard
  ('dashboard
   (dashboard-setup-startup-hook)
   (add-hook 'after-init-hook
             (lambda ()
               (weather-fetch-weather-data nil #'dashboard-refresh-buffer dashboard-buffer-name))))
  ('scratch
   (add-hook 'after-init-hook
             #'+evan/scratch-setup))
  ('enlight
   (require 'init-enlight)
   (setopt initial-buffer-choice #'enlight)))

(defun lock-*scratch*-buffer()
  "Lock *scratch* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*scratch*")
    (emacs-lock-mode 'kill)))

(add-hook 'after-init-hook #'lock-*scratch*-buffer)

(provide 'init-dashboard)
;;; init-dashboard.el ends here
