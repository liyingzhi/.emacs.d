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
       :file ,(expand-file-name "logo_black_medium.png" user-emacs-directory)))))

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
  (advice-add #'dashboard-insert-footer :after #'my-dashboard-insert-copyright))

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

  ;; Refresh dashboard buffer
  (dashboard-refresh-buffer)

  ;; Jump to the first section
  (dashboard-goto-recent-files))

(if user/dashboard
    (dashboard-setup-startup-hook)
  (add-hook 'after-init-hook
            #'+evan/scratch-setup))

(defun lock-*scratch*-buffer()
  "Lock *scratch* buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*scratch*")
    (emacs-lock-mode 'kill)))

(add-hook 'after-init-hook #'lock-*scratch*-buffer)

(provide 'init-dashboard)
;;; init-dashboard.el ends here
