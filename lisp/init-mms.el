;;; init-mms.el --- mms                            -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)

;;; emms

(add-hook 'emms-playlist-mode-hook #'meow-motion-mode)
(defvar +favorites-playlist "~/Music/fav.m3u")

(defvar emms-player-mpv-volume 30)
(defvar mms/emms-player-mpv-volume-mute nil)

(defun +emms-source-file-directory-tree-fd (dir regex)
  "Return files below DIR whose absolute names match REGEX."
  (let ((directory (expand-file-name dir)))
    (when (file-directory-p directory)
      (if-let* ((fd (executable-find "fd")))
          (with-temp-buffer
            (let ((status
                   (call-process fd nil t nil
                                 "--type" "f" "--absolute-path" "--print0"
                                 "." directory)))
              (unless (zerop status)
                (error "fd failed: %s" (string-trim (buffer-string))))
              (seq-filter
               (lambda (file) (string-match-p regex file))
               (split-string (buffer-string) "\0" t))))
        (directory-files-recursively directory regex)))))

(defun +emms-extract-embedded-cover (track)
  "Return TRACK's embedded artwork as a large cover file.

When called interactively, use the currently selected EMMS track."
  (interactive
   (list
    (if-let* ((selected-track (emms-playlist-current-selected-track)))
        (emms-track-name selected-track)
      (user-error "No selected EMMS track"))))
  (let ((cover (expand-file-name "cover_large.png"
                                 (file-name-directory track))))
    (unless (file-readable-p cover)
      (let ((ffmpeg (executable-find "ffmpeg")))
        (unless ffmpeg
          (user-error "ffmpeg not found; install it to extract embedded covers"))
        (unless (zerop
                 (call-process
                  ffmpeg nil nil nil "-nostdin" "-v" "error" "-y"
                  "-i" track "-map" "0:v:0" "-frames:v" "1" "-update" "1" cover))
          (when (file-exists-p cover) (delete-file cover)))))
    (and (file-readable-p cover) cover)))

(defun +emms-extract-embedded-covers ()
  "Extract missing large covers from embedded artwork."
  (interactive)
  (require 'emms-browser)
  (let (albums)
    (dolist (track
             (+emms-source-file-directory-tree-fd
              emms-source-file-default-directory (emms-source-file-regex)))
      (unless (member (file-name-directory track) albums)
        (push (file-name-directory track) albums)
        (+emms-extract-embedded-cover track))))
  (when (hash-table-p emms-browser--cache-hash)
    (emms-browser-clear-cache-hash))
  (when (buffer-live-p emms-browser-buffer)
    (kill-buffer emms-browser-buffer)))

(defun +emms-browser-cover (directory size)
  "Return an EMMS SIZE cover from DIRECTORY, extracting large artwork on demand."
  (if (eq size 'large)
      (let ((cover (expand-file-name "cover_large.png" directory)))
        (or (and (file-readable-p cover) cover)
            (when-let* ((track (car (directory-files
                                     directory t (emms-source-file-regex)))))
              (+emms-extract-embedded-cover track))))
    (emms-browser-cache-thumbnail-async directory size)))

(defun +emms-lyrics-find-with-info-lyric (file)
  "Find external lyric FILE, falling back to the current track's tag."
  (or (emms-lyrics-find-lyric file)
      (when-let* ((track (emms-playlist-current-selected-track))
                  (_ (or (emms-track-get track 'info-lyrics)
                         (emms-info-exiftool track)))
                  (lyrics (emms-track-get track 'info-lyrics))
                  (cache-file (expand-file-name
                               (concat (md5 (emms-track-name track)) ".lrc")
                               (expand-file-name "lyrics/" emms-directory)))
                  ((stringp lyrics))
                  ((not (string-empty-p lyrics))))
        (when (or (not (file-exists-p cache-file))
                  (file-newer-than-file-p (emms-track-name track)
                                          cache-file))
          (make-directory (file-name-directory cache-file) t)
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file cache-file
              (insert lyrics))))
        cache-file)))

(defcustom +emms-syncedlyrics-program "syncedlyrics"
  "Executable used as the fallback lyrics provider."
  :type 'string)

(defcustom +emms-syncedlyrics-providers
  '("netease" "musixmatch" "megalobiz")
  "Providers used by `syncedlyrics' after LRCLIB fails."
  :type '(repeat string))

(defcustom +emms-auto-fetch-lyrics nil
  "When non-nil, fetch synchronized lyrics when a track starts playing."
  :type 'boolean)

(defun +emms-lyrics-file-nonempty-p (file)
  "Return non-nil when FILE exists and contains data."
  (and (file-readable-p file)
       (> (file-attribute-size (file-attributes file)) 0)))

(defun +emms-syncedlyrics-sentinel (process _event)
  "Handle completion of a `syncedlyrics' PROCESS."
  (when (memq (process-status process) '(exit signal))
    (let ((file (process-get process 'emms-lyrics-file))
          (track (process-get process 'emms-lyrics-track))
          (interactive (process-get process 'emms-lyrics-interactive)))
      (if (and (= 0 (process-exit-status process))
               (+emms-lyrics-file-nonempty-p file))
          (progn
            (when interactive
              (message "Saved fallback synced lyrics at \"%s\"" file))
            (when (and (boundp 'emms-lyrics-display-p)
                       emms-lyrics-display-p
                       emms-player-playing-p
                       (equal track (emms-playlist-current-selected-track)))
              (emms-lyrics-catchup file)))
        (when interactive
          (message "No synchronized lyrics found from fallback providers."))))))

(defun +emms-lyrics-syncedlyrics-get (track file interactive)
  "Fetch TRACK's synchronized lyrics with `syncedlyrics' into FILE."
  (if-let* ((program (executable-find +emms-syncedlyrics-program))
            (query (mapconcat
                    #'identity
                    (delq nil
                          (list (emms-lyrics-lrclib-encode-name (emms-track-get track 'info-title))
                                (emms-lyrics-lrclib-encode-name (emms-track-get track 'info-artist))
                                ;; (emms-lyrics-lrclib-encode-name (emms-track-get track 'info-album))
                                ))
                    " "))
            (process
             (apply #'start-process
                    (format "emms-syncedlyrics-%s" (float-time))
                    nil
                    program
                    (append (list "-p")
                            +emms-syncedlyrics-providers
                            (list "-o" file "--synced-only" query)))))
      (progn
        (process-put process 'emms-lyrics-file file)
        (process-put process 'emms-lyrics-track track)
        (process-put process 'emms-lyrics-interactive interactive)
        (set-process-sentinel process #'+emms-syncedlyrics-sentinel)
        (when interactive
          (message "LRCLIB had no result; trying alternate lyric providers...")))
    (when interactive
      (message "Fallback unavailable: install the `syncedlyrics' command."))))

(defun +emms-lyrics-lrclib-parse (status file track interactive)
  "Parse LRCLIB response and fall back to other providers when needed."
  (let ((existing (file-exists-p file)))
    (condition-case err
        (emms-lyrics-lrclib-parse status file track interactive)
      (error
       (when interactive
         (message "LRCLIB request failed: %s"
                  (error-message-string err)))))
    (unless (or existing (+emms-lyrics-file-nonempty-p file))
      (+emms-lyrics-syncedlyrics-get track file interactive))))

(defun +emms-lyrics-lrclib-get (&optional track force interactive)
  "Fetch synchronized lyrics for TRACK from LRCLIB.

This is the EMMS LRCLIB command with optional album metadata.  LRCLIB
can match a track using its title, artist, and duration when the album
tag is missing."
  (if (> emms-lyrics-lrclib-requests emms-lyrics-lrclib-max-requests)
      (emms-later-do #'+emms-lyrics-lrclib-get track force interactive)
    (when-let* ((track (or track (emms-playlist-current-selected-track)))
                ((eq (emms-track-type track) 'file))
                (file (emms-track-name track))
                (lrc (replace-regexp-in-string "\\.[^.]+\\'" ".lrc" file))
                ((or force (not (file-exists-p lrc))))
                ((file-writable-p lrc))
                (title (emms-lyrics-lrclib-encode-name
                        (emms-track-get track 'info-title)))
                (artist (emms-lyrics-lrclib-encode-name
                         (emms-track-get track 'info-artist)))
                (time (emms-track-get track 'info-playing-time)))
      (let ((album (emms-lyrics-lrclib-encode-name
                    (emms-track-get track 'info-album))))
        (setq emms-lyrics-lrclib-requests
              (1+ emms-lyrics-lrclib-requests))
        (when interactive (message "Searching for lyrics..."))
        (url-retrieve
         (url-encode-url
          (format "%sget?artist_name=%s&track_name=%s%s&duration=%d"
                  emms-lyrics-lrclib-url artist title
                  (if (and album (not (string-empty-p album)))
                      (format "&album_name=%s" album)
                    "")
                  time))
         #'+emms-lyrics-lrclib-parse (list lrc track interactive))))))

(defun mms/emms--maybe-fetch-lyrics ()
  "Fetch lyrics for the current track when `+emms-auto-fetch-lyrics' is set."
  (when +emms-auto-fetch-lyrics
    (when-let* ((track (emms-playlist-current-selected-track)))
      (+emms-lyrics-lrclib-get track nil nil))))


;;; setting
(with-eval-after-load 'emms
  (require 'emms-setup)
  (require 'emms-mpris)
  (require 'emms-lyrics-lrclib)
  (advice-add 'emms-lyrics-lrclib-get
              :override #'+emms-lyrics-lrclib-get)
  ;; EMMS still assumes list timestamps; Emacs may return (TICKS . HZ).
  (defalias 'mms--emms-time-less-p #'time-less-p)
  (advice-add 'emms-time-less-p :override #'mms--emms-time-less-p)

  ;; (emms-default-players)
  (setq emms-player-list '(emms-player-mpv)
        emms-player-mpv-parameters '("--quiet" "--no-video" "--force-window=no"))

  (setq emms-source-file-default-directory "~/Music")
  (setq +favorites-playlist (concat emms-source-file-default-directory "/fav.m3u"))

  ;; covers
  (setq emms-browser-covers #'+emms-browser-cover
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128
        emms-info-functions '(emms-info-exiftool)
        emms-track-description-function #'emms-info-track-description
        emms-lyrics-find-lyric-function #'+emms-lyrics-find-with-info-lyric
        emms-lyrics-scroll-p nil
        emms-source-file-directory-tree-function #'+emms-source-file-directory-tree-fd
        emms-playlist-buffer-name "*Music*"
        emms-playlist-mode-center-when-go t
        emms-info-asynchronously t
        emms-info-auto-update t
        emms-volume-change-amount 5
        emms-volume-change-function #'mms/emms-volume-mpv-change
        emms-show-format "♪ %s")

  (setopt emms-player-mpv-update-metadata t)

  (emms-all)
  (emms-mpris-enable)
  ;; history
  ;; (emms-history-load)

  (emms-cache 1)

  ;;;###autoload
  (defun +emms-add-to-favorites ()
    "Add the current track to the favorites playlist.
The track is added to the playlist file specified by `+favorites-playlist'.
If the track already exists in the playlist, it won't be duplicated."
    (interactive)
    (emms-playlist-mode-center-current)
    (+emms-playlist-save +favorites-playlist))

  (defun mms/emms--playlist-contains-path-p (file path)
    "Return non-nil when FILE already lists PATH as an m3u entry."
    (let ((playlist-dir (file-name-directory file)))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (cl-loop
         while (not (eobp))
         for line = (string-trim
                     (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))
         when (and (not (string-empty-p line))
                   (not (string-prefix-p "#" line))
                   (string= (expand-file-name line playlist-dir) path))
         return t
         do (forward-line 1)))))

  (defun mms/emms--file-ends-with-newline-p (file)
    "Return non-nil when FILE is empty or ends with a newline."
    (let ((size (file-attribute-size (file-attributes file))))
      (or (zerop size)
          (with-temp-buffer
            (insert-file-contents file nil (1- size) size)
            (eq (char-after (point-min)) ?\n)))))

  ;;;###autoload
  (defun +emms-playlist-save (file)
    "Append the track at point to playlist FILE as an m3u entry.
FILE is created with an `#EXTM3U' header when missing.  Duplicate
paths (exact line match after expand) are skipped."
    (interactive (list (read-file-name "Store as: "
                                       emms-source-file-default-directory)))
    (let* ((track (emms-playlist-track-at (point)))
           (path (expand-file-name
                  (or (emms-track-get track 'name)
                      (emms-track-force-description track))))
           (file (expand-file-name file))
           (exists (file-exists-p file)))
      (if (and exists (mms/emms--playlist-contains-path-p file path))
          (message "Playlist already contains the same entries. Not saving.")
        (let ((coding-system-for-write 'utf-8-unix)
              (need-newline (and exists
                                 (not (mms/emms--file-ends-with-newline-p file)))))
          (with-temp-buffer
            (unless exists
              (insert "#EXTM3U\n"))
            (when need-newline
              (insert "\n"))
            (insert path "\n")
            (write-region (point-min) (point-max) file exists))))))

  ;; mpv integration
  ;; https://www.reddit.com/r/emacs/comments/syop1h/control_emmsmpv_volume/
  (defun mms/emms--mpv-set-initial-volume ()
    "Apply `emms-player-mpv-volume' once per mpv IPC connection."
    (emms-player-mpv-cmd `(set_property volume ,emms-player-mpv-volume)))

  (add-hook 'emms-player-mpv-event-connect-hook #'mms/emms--mpv-set-initial-volume)

  (defun mms/emms-mpv-get-volume ()
    "Sync `emms-player-mpv-volume' from mpv and refresh the transient."
    (emms-player-mpv-cmd '(get_property volume)
                         #'(lambda (vol err)
                             (unless err
                               (let ((vol (truncate vol)))
                                 (setq emms-player-mpv-volume vol)
                                 (message "Music volume: %s%%" vol)
                                 (mms/transient-emms--refresh-volume))))))

  (defun mms/emms-volume-mpv-change (amount)
    "Change mpv volume by AMOUNT, respecting mute and updating UI state."
    (unless mms/emms-player-mpv-volume-mute
      (let ((new-volume (+ emms-player-mpv-volume amount)))
        (cond ((> new-volume 100)
               (emms-player-mpv-cmd '(set_property volume 100)))
              ((< new-volume 0)
               (emms-player-mpv-cmd '(set_property volume 0)))
              (t
               (emms-player-mpv-cmd `(add volume ,amount)))))
      (mms/emms-mpv-get-volume)))

  (defun mms/emms-mpv-zero-volume ()
    "Set the volume of the MPV player to zero."
    (interactive)
    (emms-player-mpv-cmd '(set_property volume 0))
    (mms/emms-mpv-get-volume))

  (defun mms/emms-mpv-mute-volume ()
    "Toggle mute status of the MPV player.
If currently muted, restore previous volume; otherwise set volume to zero."
    (interactive)
    (if mms/emms-player-mpv-volume-mute
        (progn
          (emms-player-mpv-cmd `(set_property volume ,emms-player-mpv-volume))
          (setq mms/emms-player-mpv-volume-mute nil))
      (emms-player-mpv-cmd '(set_property volume 0))
      (setq mms/emms-player-mpv-volume-mute t))
    (mms/transient-emms--refresh-volume))

  (defun mms/emms--volumes-description ()
    "Return a formatted string describing the current volume for display in menu."
    (format (propertize "Volume: %s" 'face 'transient-heading)
            (if mms/emms-player-mpv-volume-mute
                (propertize (format "Mute")
                            'face
                            'transient-value)
              (propertize (format "%s  " emms-player-mpv-volume)
                          'face
                          'transient-value))))

  (defun mms/transient-emms--refresh-volume ()
    "Update volume information when transient menu are present."
    (when transient--suffixes
      (transient-setup 'mms/transient-emms)))

  (with-eval-after-load 'emms-info-exiftool
    (add-to-list 'emms-info-exiftool-field-map '(info-lyrics . Lyrics)))

  ;; extract track info when loading the playlist
  (push 'emms-info-initialize-track emms-track-initialize-functions)

  (add-hook 'emms-player-started-hook #'emms-show)
  (add-hook 'emms-player-started-hook #'mms/emms--maybe-fetch-lyrics)
  (emms-mode-line-mode -1)

  (keymap-sets emms-playlist-mode-map
    '(("C-o" . mms/transient-emms)
      ("F" . +emms-add-to-favorites)
      ("j" . next-line)
      ("k" . previous-line)
      ("N" . emms-ui-now-playing)
      ("L" . emms-ui-list)
      ("A" . emms-ui-albums)
      ("U" . emms-ui)
      ("SPC" . emms-pause))))

;;; select roi songs
(defun mms/filter-music-buffer-and-save-to-file (json-filepath output-filepath)
  "If current buffer is named `emms-playlist-buffer-name', read JSON file JSON-FILEPATH to extract titles.
then prompt user to continue. If user answers yes, filter current buffer to collect matching lines,
then write results to OUTPUT-FILEPATH, one element per line."
  (interactive
   (list (read-file-name "JSON file path: ")
         (read-file-name "Save results to file (output path): ")))
  ;; Check buffer name
  (unless (string= (buffer-name) emms-playlist-buffer-name)
    (user-error "Current buffer is not Emms-Playlist-Buffer, operation cancelled"))
  ;; Extract title list
  (let ((my-roi-song-names (extract-song-titles-from-file json-filepath)))
    (unless (listp my-roi-song-names)
      (error "Failed to extract title list from JSON"))
    ;; Prompt user to continue
    (when (y-or-n-p (format "Extracted %d titles from %s. Continue filtering current buffer? "
                            (length my-roi-song-names) json-filepath))
      ;; Collect matching lines
      (let ((matched-lines (collect-lines-containing-substrings my-roi-song-names)))
        ;; Write to output file
        (with-temp-buffer
          (dolist (line matched-lines)
            (insert line "\n"))
          (write-region (point-min) (point-max) output-filepath))
        (message "Filtering completed, %d lines written to %s" (length matched-lines) output-filepath)))))


;;; menu

(defun mms/emms-play-default-playlist (&optional arg)
  "Play `user/mms-playlist-file'.
With prefix ARG, or when that file is missing, prompt for a playlist."
  (interactive "P")
  (if (or arg
          (not (and (stringp user/mms-playlist-file)
                    (file-exists-p user/mms-playlist-file))))
      (call-interactively #'emms-play-playlist)
    (emms-play-playlist user/mms-playlist-file)))

;; autoload
(autoload #'emms "emms" nil t)
(autoload #'emms-pause "emms" nil t)
(autoload #'emms-history-load "emms-history" nil t)
(autoload 'emms-info-exiftool "emms-info-exiftool")
;;** EMMS helpers
;; transient to control EMMS
;; https://tech.toryanderson.com/2023/11/29/transient-for-convenience-with-emms/
;; https://github.com/ifinkelstein/dotemacs/blob/96f9d0e12ccf06d8a0d5dfebf22b98a3daf405a1/library/setup/my-setup-media.el#L82
(transient-define-prefix mms/transient-emms ()
  "EMMS music"
  :transient-non-suffix 'transient--do-stay
  ["EMMS"
   ["Controls"
    :pad-keys t
    ("P" "Play/Pause" emms-pause :transient t)
    ("S" "Stop" emms-stop :transient t)
    ("s" "Seek to time" emms-seek-to :transient t)
    (">" "Next" emms-next :transient t)
    ("<" "Back (Previous)" emms-previous :transient t)
                                        ; I want the transient to stay open on just these commands, so I can easily repeat them
    ("b" "Back rewind" emms-seek-backward :transient t)
    ("f" "Fast-Forward" emms-seek-forward :transient t)]
   ["Playlist"
    :pad-keys t
    ("h" "History" emms-history-load)
    ("L" "Load playlist" mms/emms-play-default-playlist)
    ("%" "Sort playlist" emms-sort :transient t)
    ("O" "Random track" emms-random :transient t)
    ("R" "Toggle shuffle" emms-toggle-random-playlist :transient t)
    ("o" "Toggle repeat track" emms-toggle-repeat-track :transient t)
    ("r" "Toggle repeat list" emms-toggle-repeat-playlist :transient t)
    ;; ("N" "Cue Next" emms-cue-next :transient t)
    ;; ("P" "Cue Previous" emms-cue-previous :transient t)
    ]
   [:description
    mms/emms--volumes-description
    :pad-keys t
    ("m" "Mute" mms/emms-mpv-mute-volume :transient t)
    ("z" "Zero" mms/emms-mpv-zero-volume :transient t)
    ("=" "Vol+" emms-volume-mode-plus :transient t)
    ("-" "Vol-" emms-volume-mode-minus :transient t)]
   ["Favorites"
    :pad-keys t
    ("l" "Load fav playlist" (lambda ()
                               (interactive)
                               (emms-play-playlist +favorites-playlist)))
    ("E" "Filter roi and Export" mms/filter-music-buffer-and-save-to-file)
    ("G" "Get entry to fav" +emms-add-to-favorites :transient t)
    ("g" "Goto entry line" consult-emms-current-playlist)]
   ["Global/External"
    :pad-keys t
    ("d" "Emms mark with dired" emms-play-dired)
    ("D" "Emms play directory" emms-play-directory)
    ("t" "Emms add dir tree" emms-add-directory-tree)
    ("F" "Emms play find" emms-play-find)
    ;; ("u" "Music dir" tsa/jump-to-music) ;; invokes a bookmark, which in turn hops to my bookmarked music directory
    ;; ("M" "   Modeline" emms-mode-line-mode)
    ("I" "Current info" emms-show)
    ("e" "Emms" emms)]])

;;; keymap

(global-set-keys
 `(("C-c m b" . emms-smart-browse)
   ("C-c m C" . +emms-extract-embedded-covers)
   ("C-c m c" . +emms-extract-embedded-cover)
   ("C-c m e" . emms)
   ("C-c m l" . emms-lyrics-lrclib-get)
   ("C-c m L" . emms-lyrics-visit-lyric)
   ("C-c m o" . mms/transient-emms)
   ("C-c m p" . ("emms-play-playlist" . mms/emms-play-default-playlist))
   ("C-c m f" . ("emms-filter-playlist" .
                 ,(lambda ()
                    "Filter EMMS playlist interactively.
Only works when current buffer is the EMMS playlist buffer."
                    (interactive)
                    (if (bound-and-true-p emms-playlist-buffer-name)
                        (if (string= emms-playlist-buffer-name (buffer-name))
                            (call-interactively #'filter-lines-containing-and-save)
                          (message "Current buffer is not %s" emms-playlist-buffer-name))
                      (message "Not exists EMMS buffer")))))
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)
   ("<XF86AudioMute>" . mms/emms-mpv-mute-volume)
   ("<XF86AudioPause>" . emms-pause)
   ("<XF86AudioRaiseVolume>" . emms-volume-mode-plus)
   ("<XF86AudioLowerVolume>" . emms-volume-mode-minus)))

(which-key-add-key-based-replacements
  "C-c m" "Multimedia")

(defun tab-bar-switch-or-create-music (&optional arg)
  "Create or switch music tab bar.
With prefix argument ARG, start ytm-radio instead of emms."
  (interactive "P")
  (autoload 'tab-bar-switch-or-create "lib-tabbar" nil t)
  (tab-bar-switch-or-create "Music")
  (if arg
      (prog1 (ytm-radio)
        (when (> (count-windows) 1)
          (toggle-delete-other-windows)
          (ytm-radio-refresh)))
    (if (bound-and-true-p emms-playlist-buffer)
        (emms-playlist-mode-go)
      (emms-history-load)
      (if (and (stringp emms-history-file)
               (file-exists-p emms-history-file))
          (emms-playlist-mode-go)
        (emms)
        (mms/emms-play-default-playlist)))))

(global-bind-keys
 ("C-c l s" . ("Music Tab emms" . tab-bar-switch-or-create-music))
 ("C-c l y" . ("Music Tab ytm" . (lambda ()
                                   (interactive)
                                   (tab-bar-switch-or-create-music t)))))

;;; ready-player
(setopt ready-player-minor-mode-map-prefix "C-c m r")
;; (ready-player-mode)

;;; ytm-radio
(with-eval-after-load 'ytm-radio
  (when user/*ytm-radio-default-font*
    (set-font-for-modes
     `((ytm-radio--mode . ,user/*ytm-radio-default-font*))))
  (setq ytm-radio-auto-show-now-playing nil)

  (keymap-unset ytm-radio--mode-map "g")
  (keymap-sets ytm-radio--mode-map
    '(("G" . ytm-radio-refresh)
      ("h" . ytm-radio-back)
      ("C-o" . ytm-radio-current-actions)
      ("g i" . consult-imenu))))

(global-set-keys
 `(("C-c m y" . ytm-radio)))
;;; emms-ui
(setopt emms-ui-album-cover-size 220
        emms-ui-track-columns '(status info-title info-playing-time info-artist
                                       info-album info-genre info-year play-count)
        emms-ui-now-playing-cover-max-size 640
        emms-ui-now-playing-default-view 'cover)

(with-eval-after-load 'emms-ui
  (keymap-binds emms-ui-now-playing-mode-map
    (("+" "=") . emms-volume-mode-plus)
    ("-" . emms-volume-mode-minus)
    ("A" . emms-ui-albums)
    ("E" . emms))
  (keymap-binds emms-ui-albums-mode-map
    (("+" "=") . emms-volume-mode-plus)
    ("-" . emms-volume-mode-minus)
    ("A" . emms-ui-list)
    ("E" . emms))
  (keymap-binds emms-ui-list-mode-map
    (("+" "=") . emms-volume-mode-plus)
    ("-" . emms-volume-mode-minus)
    ("A" . emms-ui-albums)
    ("E" . emms)))

;;; consult-emms
(setq consult-emms--sort-album-function #'string<)
(global-set-keys
 `(("C-c m a" . consult-emms-library)
   ("C-c m j" . consult-emms-current-playlist)))

(provide 'init-mms)
;;; init-mms.el ends here
