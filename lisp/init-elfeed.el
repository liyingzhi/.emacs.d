;;; init-elfeed.el --- elfeed setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(wait-packages!
 '(elfeed
   elfeed-tube
   elfeed-tube-mpv
   elfeed-org))


(with-eval-after-load 'elfeed
  ;; Load elfeed-org
  (require 'elfeed-org)

  ;; Initialize elfeed-org
  ;; This hooks up elfeed-org to read the configuration when elfeed
  ;; is started with =M-x elfeed=
  (elfeed-org)

  ;; Optionally specify a number of files containing elfeed
  ;; configuration. If not set then the location below is used.
  ;; Note: The customize interface is also supported.
  (setopt rmh-elfeed-org-files (list "~/Documents/Org/elfeed.org"))

  (setopt url-queue-timeout 30)

  (add-hook 'elfeed-show-mode-hook #'visual-line-mode)

  (require 'lib-elfeed)
  (setq elfeed-search-print-entry-function #'+elfeed-search-print-entry--better-default)

  (require 'elfeed-tube)
  (setopt elfeed-tube-backend 'yt-dlp)
  (setopt elfeed-tube-captions-languages
          '("zh" "en" "english (auto generated)")
          ;; mpv-default-options '("--http-proxy=http://127.0.0.1:7897"
          ;;                       "--ytdl-raw-options-append=proxy=http://127.0.0.1:7897")
          )
  (elfeed-tube-setup))

(autoload #'tab-bar-switch-or-create-rss "lib-elfeed" nil t)

(global-bind-keys
 ("C-c l r" . ("Rss Tab" . tab-bar-switch-or-create-rss)))

(with-eval-after-load 'elfeed-show
  (keymap-sets elfeed-show-mode-map
    '(("C-c C-o" . +open-link-with-mpv)
      ("C-c C-f" . elfeed-tube-fetch)
      ("C-c C-l" . elfeed-tube-mpv-follow-mode)
      ("C-c C-w" . elfeed-tube-mpv-where)
      ("C-c C-m" . elfeed-tube-mpv)
      ("C-x C-s" . elfeed-tube-save))))

(with-eval-after-load 'elfeed-search
  (keymap-sets elfeed-search-mode-map
    '(("l" . +elfeed-overview)
      ("F" . elfeed-tube-fetch)
      ("C-x C-s" . elfeed-tube-save))))

(provide 'init-elfeed)
;;; init-elfeed.el ends here
