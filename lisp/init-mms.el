;;; init-mms.el --- mms                            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; emms

(add-hook 'emms-playlist-mode-hook #'meow-motion-mode)
(defvar +favorites-playlist "~/Music/fav.m3u")

(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'emms-mpris)

  (setq emms-playlist-buffer-name "*Music*")
  ;; (emms-default-players)
  (setq emms-player-list '(emms-player-mpv emms-player-vlc))
  (setq emms-source-file-default-directory "~/Music")
  (setq +favorites-playlist (concat emms-source-file-default-directory "/fav.m3u"))

  ;; covers
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  (setq emms-browser-thumbnail-small-size 100)
  (setq emms-browser-thumbnail-medium-size 200)

  (setq emms-show-format "Playing: %s")

  (emms-all)
  (emms-mpris-enable)
  ;; history
  ;; (emms-history-load)

  (defun +emms-select-song ()
    "Select and play a song from the current EMMS playlist."
    (interactive)
    (with-current-emms-playlist
      (emms-playlist-mode-center-current)
      (let* ((current-line-number (line-number-at-pos))
             (lines (cl-loop
                     with min-line-number = (line-number-at-pos (point-min))
                     with buffer-text-lines = (split-string (buffer-string) "\n")
                     with lines = nil
                     for l in buffer-text-lines
                     for n = min-line-number then (1+ n)
                     do (push (cons l n)
                              lines)
                     finally return (nreverse lines)))
             (selected-line (completing-read "Song: " lines)))
        (when selected-line
          (let ((line (cdr (assoc selected-line lines))))
            (goto-line line)
            (emms-playlist-mode-play-smart)
            (emms-playlist-mode-center-current))))))

  ;;;###autoload
  (defun +emms-add-to-favorites ()
    "Add the current song to the hard-coded favorites playlist."
    (interactive)
    (save-window-excursion
      (progn
        (with-current-buffer emms-playlist-buffer-name
          (emms-playlist-mode-add-contents))
        (+emms-playlist-save 'm3u +favorites-playlist t)

        (setq emms-playlist-buffer
              (emms-playlist-set-playlist-buffer
               (get-buffer emms-playlist-buffer-name))))))

;;;###autoload
  (defun +emms-playlist-save (format file option &optional kill-bufer)
    "Save the current EMMS playlist to FILE in the specified FORMAT.
If KILL-BUFEER is non-nil, the playlist buffer will be killed after saving.
The default format is determined by `emms-source-playlist-default-format'."
    (interactive (list (emms-source-playlist-read-format)
                       (read-file-name "Store as: "
                                       emms-source-file-default-directory
                                       emms-source-file-default-directory
                                       nil)))
    (with-temp-buffer (emms-source-playlist-unparse format
                                                    (with-current-emms-playlist
                                                      (current-buffer))
                                                    (current-buffer))
                      (let ((append-to-file t))
                        (write-region (point-min) (point-max) file t))

                      (when kill-bufer
                        (with-current-emms-playlist
                          (kill-buffer)))))

  ;; mpv integration
  ;; https://www.reddit.com/r/emacs/comments/syop1h/control_emmsmpv_volume/
  (defvar emms-player-mpv-volume 100)

  (defun emms-player-mpv-get-volume ()
    "Sets `emms-player-mpv-volume' to the current volume value
and sends a message of the current volume status."
    (emms-player-mpv-cmd '(get_property volume)
                         #'(lambda (vol err)
                             (unless err
                               (let ((vol (truncate vol)))
                                 (setq emms-player-mpv-volume vol)
                                 (message "Music volume: %s%%"
                                          vol))))))

  (defun emms-player-mpv-raise-volume (&optional amount)
    "Raise the volume of the MPV player by AMOUNT.
If AMOUNT is not provided, it defaults to 10.
The volume will not exceed 100."
    (interactive)
    (let* ((amount (or amount 10))
           (new-volume (+ emms-player-mpv-volume amount)))
      (if (> new-volume 100)
          (emms-player-mpv-cmd '(set_property volume 100))
        (emms-player-mpv-cmd `(add volume ,amount))))
    (emms-player-mpv-get-volume))

  (defun emms-player-mpv-lower-volume (&optional amount)
    "Lower the volume of the MPV player by AMOUNT.
If AMOUNT is not provided, it defaults to 10."
    (interactive)
    (emms-player-mpv-cmd `(add volume ,(- (or amount '10))))
    (emms-player-mpv-get-volume))

  (emms-player-mpv-cmd '(set_property volume 30))

                                        ; `emms-info-native' supports mp3,flac,ogg and requires NO CLI tools
  (unless (memq 'emms-info-native emms-info-functions)
    (require 'emms-info-native)
    (push 'emms-info-native emms-info-functions))

  ;; extract track info when loading the playlist
  (push 'emms-info-initialize-track emms-track-initialize-functions)

  (add-hook 'emms-player-started-hook 'emms-show)
  (emms-mode-line-mode -1)
  (advice-add 'emms-volume-raise :override #'emms-player-mpv-raise-volume)
  (advice-add 'emms-volume-lower :override #'emms-player-mpv-lower-volume)

  (keymap-sets emms-playlist-mode-map
    '(("C-o" . my/transient-emms))))

;;** EMMS helpers
;; transient to control EMMS
;; https://tech.toryanderson.com/2023/11/29/transient-for-convenience-with-emms/

(transient-define-prefix my/transient-emms ()
  "EMMS music"
  :transient-non-suffix 'transient--do-stay
  ["EMMS"
   ["Controls"
    ("p" "⏯ Play/Pause" emms-pause :transient t)
    ("S" "⏹ Stop" emms-stop :transient t)
    ("t" "⏲ Seek to time" emms-seek-to :transient t)
    (">" "⏭ Next" emms-next :transient t)
    ("<" "⏮ Back (Previous)" emms-previous :transient t)
                                        ; I want the transient to stay open on just these commands, so I can easily repeat them
    ("b" "⏪ Back rewind" emms-seek-backward :transient t)
    ("f" "⏩ Fast-Forward" emms-seek-forward :transient t)]

   ["Playlist"

    ("L" " Load playlist" (lambda ()
                             (interactive)
                             (emms-play-playlist user/mms-playlist-file)))
    ("%" " Sort playlist" emms-sort :transient t)
    ("R o" "🔀 play Random" emms-random :transient t)
    ("R a" "🔀 toggle shuffle" emms-toggle-random-playlist :transient t)
    ("r o" "🔁 toggle repeat t" emms-toggle-repeat-track :transient t)
    ("r a" "🔁 toggle repeat p" emms-toggle-repeat-playlist :transient t)
    ;; ("N" "Cue Next" emms-cue-next :transient t)
    ;; ("P" "Cue Previous" emms-cue-previous :transient t)
    ]

   ["Global/External"
    ("d" "📂 emms Dired" emms-play-dired)
    ;; ("u" "Music dir" tsa/jump-to-music) ;; invokes a bookmark, which in turn hops to my bookmarked music directory
    ;; ("m" "   Modeline" emms-mode-line-mode)
    ("M" "🔍 current info" emms-show)
    ("e" "🎵 emms" emms)
    ]

   ["Favorites"
    ("l" " load" (lambda ()
                    (interactive)
                    (emms-play-playlist +favorites-playlist)))
    ("s" " save" +emms-add-to-favorites :transient t)
    ("g" " goto" +emms-select-song)
    ("h" " history" emms-history-load)]

   ["Volume"
    ("=" "  Vol+" emms-player-mpv-raise-volume :transient t)
    ("-" "  Vol-" emms-player-mpv-lower-volume :transient t)]
   ])

;; autoload
(autoload #'emms "emms" nil t)
(autoload #'emms-pause "emms" nil t)

(global-set-keys
 `(("C-c m b" . emms-browser)
   ("C-c m e" . emms)
   ("C-c m o" . my/transient-emms)
   ("C-c m p" . ("emms-play-playlist" . ,(lambda (arg)
                                           (interactive "P")
                                           (if arg
                                               (call-interactively #'emms-play-playlist)
                                             (if (and user/mms-playlist-file
                                                      (file-exists-p user/mms-playlist-file))
                                                 (emms-play-playlist user/mms-playlist-file)
                                               (call-interactively #'emms-play-playlist))))))
   ("C-c m f" . ("emms-filter-playlist" . ,(lambda ()
                                             (interactive)
                                             (if (bound-and-true-p emms-playlist-buffer-name)
                                                 (if (string= emms-playlist-buffer-name (buffer-name))
                                                     (call-interactively #'filter-lines-containing-and-save)
                                                   (message "Current buffer is not %s" emms-playlist-buffer-name))
                                               (message "Not exists EMMS buffer")))))
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))



(provide 'init-mms)
;;; init-mms.el ends here
