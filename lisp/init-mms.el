;;; init-mms.el --- mms                            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; emms

(setq emms-source-file-default-directory "~/Music")
(setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
;; (emms-default-players)
(setq emms-player-list '(emms-player-mpv))

(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-mpris-enable))

(add-hook 'emms-playlist-mode-hook #'meow-motion-mode)

(global-set-keys
 `(("C-c m b" . emms-browser)
   ("C-c m e" . emms)
   ("C-c m p" . ,(lambda ()
                   (interactive)
                   (if (and user/mms-playlist-file (file-exists-p user/mms-playlist-file))
                       (emms-play-playlist user/mms-playlist-file)
                     (call-interactively #'emms-play-playlist))))

   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

(provide 'init-mms)
;;; init-mms.el ends here
