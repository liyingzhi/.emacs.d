;;; lib-elfeed.el --- elfeed                         -*- lexical-binding: t; -*-

;; Copyright (C) 2026  lizqwer scott

;; Author: lizqwer scott <lizqwerscott@gmail.com>
;; Keywords: tools

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
(require 'elfeed)
(require 'lib-tabbar)
(require 'elfeed-tube)
(require 'elfeed-tube-mpv)

(defun +elfeed-overview ()
  "Get an overview of all feeds."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (elfeed-save-excursion
      (let* ((inhibit-read-only t)
             (standard-output (current-buffer)))
        (erase-buffer)
        (+elfeed-overview--update-list)
        (dolist (entry elfeed-search-entries)
          (funcall elfeed-search-print-entry-function entry)
          (insert "\n"))
        (setf elfeed-search-last-update (float-time))))
    (when (zerop (buffer-size))
      ;; If nothing changed, force a header line update
      (force-mode-line-update))
    (run-hooks 'elfeed-search-update-hook)))

(defun +elfeed-overview--update-list ()
  "Update `elfeed-search-filter' list.
Get the latest content of each elfeed entry."
  (let* ((head (list nil))
         (tail head)
         (count 0))
    (dolist (feed elfeed-feeds)
      (let* ((lexical-binding t)
             (filter (elfeed-search-parse-filter
                      (concat "=" (or (car-safe feed)
                                      feed))))
             (func (byte-compile (elfeed-search-compile-filter filter))))
        (with-elfeed-db-visit
          (entry feed)
          (when (funcall func entry feed count)
            (setf (cdr tail) (list entry)
                  tail (cdr tail)
                  count (1+ count))
            (elfeed-db-return)))))
    (let ((entries (cdr head))
          (elfeed-search-sort-function
           (lambda (a b)
             (let ((a-date (elfeed-entry-date a))
                   (b-date (elfeed-entry-date b)))
               (> a-date b-date)))))
      (setf entries (sort entries elfeed-search-sort-function))
      (setf elfeed-search-entries
            entries))))

(defun nerd-icon-for-tags (tags)
  "Generate a Nerd Font icon based on TAGS.

TAGS is a list of strings representing tags associated with an item.
The function returns a specific Nerd Font icon corresponding to
certain tags, with a default icon if no specific match is found."
  (cond ((member "youtube" tags)  (nerd-icons-faicon "nf-fa-youtube_play" :face '(:foreground "#FF0200")))
        ((member "instagram" tags) (nerd-icons-faicon "nf-fa-instagram" :face '(:foreground "#FF00B9")))
        ((member "emacs" tags) (nerd-icons-sucicon "nf-custom-emacs" :face '(:foreground "#9A5BBE")))
        ((member "economics" tags) (nerd-icons-mdicon "nf-md-alpha_e_box_outline" :face '(:foreground "#E3120C")))
        ((member "database" tags) (nerd-icons-devicon "nf-dev-database" :face '(:foreground "#0574E8")))
        ((member "bilibili" tags) (nerd-icons-mdicon "nf-md-television_classic" :face '(:foreground "#008BBE")))
        ((member "novel" tags) (nerd-icons-faicon "nf-fa-book" :face '(:foreground "#02C298")))
        ((member "forum" tags) (nerd-icons-faicon "nf-fa-forumbee" :face '(:foreground "#EF9120")))
        ((member "github" tags) (nerd-icons-faicon "nf-fa-github"))
        ((member "sourcehut" tags) (nerd-icons-faicon "nf-fa-circle_o"))
        ((member "blog" tags) (nerd-icons-faicon "nf-fa-blog"))
        (t (nerd-icons-faicon "nf-fae-feedly" :face '(:foreground "#2AB24C")))))

(defun +elfeed-search-print-entry--better-default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (date-width (car (cdr elfeed-search-date-format)))
         (title (concat (or (elfeed-meta entry :title)
                            (elfeed-entry-title entry) "")
                        ;; NOTE: insert " " for overlay to swallow
                        " "))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat (lambda (s) (propertize s 'face 'elfeed-search-tag-face)) tags ","))
         (title-width (- (frame-width)
                         ;; (window-width (get-buffer-window (elfeed-search-buffer) t))
                         date-width elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width) :left))


         ;; Title/Feed ALIGNMENT
         (align-to-feed-pixel (+ date-width
                                 (max elfeed-search-title-min-width
                                      (min title-width elfeed-search-title-max-width)))))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title))
    (put-text-property (1- (point)) (point) 'display `(space :align-to ,align-to-feed-pixel))
    ;; (when feed-title (insert " " (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when feed-title
      (insert " " (concat (nerd-icon-for-tags tags) " ")
              (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags (insert "(" tags-str ")"))))


(defun +elfeed-switch-to-log-buffer ()
  "As name suggested."
  (interactive)
  (switch-to-buffer (elfeed-log-buffer)))


(defun +open-link-with-mpv ()
  "Open the link at point with mpv if it is a video."
  (interactive)
  (let ((url (or (elfeed-get-link-at-point)
                 (elfeed-get-url-at-point))))
    (if (and url (string-match "\\(?:\\.\\(mp4\\|webm\\|ogg\\|avi\\|mkv\\)\\)?" url))
        (progn
          (message "%s" (propertize "Starting mpv, please wait!" 'face 'elfeed-log-info-level-face))
          (mpv-play-url url))
      (message "%s" (propertize "Not a video link!" 'face 'elfeed-log-warn-level-face)))))

(defun +elfeed-tube-download (entries)
  "Download YouTube videos from Elfeed entries.

ENTRIES is a list of Elfeed entries to be downloaded.  This function
works in both `elfeed-search-mode` and `elfeed-show-mode`.  It uses
`yt-dlp` to download videos to the `~/Downloads/` directory.

When called interactively, it prompts the user to download the entry
at point or the selected entries, depending on the current mode.  It
only processes entries that are identified as YouTube URLs.

For each entry, a new process is started to download the video using
`yt-dlp`, and a message is shown upon starting and completing each
download.  If the entry is not a YouTube URL, a message is displayed
and the download is cancelled."
  (interactive
   (list
    (cond
     ((eq major-mode 'elfeed-search-mode)
      (when (y-or-n-p "Download entry at point or selected entries?")
        (ensure-list (elfeed-search-selected))))
     ((eq major-mode 'elfeed-show-mode)
      (when (y-or-n-p "Download entry at point or selected entries?")
        (ensure-list elfeed-show-entry)))
     (t (user-error "elfeed-tube-download only works in Elfeed.")))))
  (when entries
    (if-let* (((seq-every-p #'elfeed-tube--youtube-p entries))
              (default-directory "~/Downloads/"))
        (seq-doseq (entry entries)
          (let* ((title (elfeed-entry-title entry))
                 (link  (elfeed-entry-link entry))
                 (proc (start-process
                        (format "yt-dlp download: %s" title)
                        (get-buffer-create (format "*elfeed-tube-yt-dlp*: %s" title))
                        "yt-dlp" "-w" "-c" "-o" "%(title)s.%(ext)s" "-f"
                        "bestvideo[height<=?720]+bestaudio/best" "--add-metadata" link)))
            (set-process-sentinel
             proc
             (lambda (process s)
               (unless (process-live-p process)
                 (if (eq (process-exit-status process) 0)
                     (progn
                       (message "Finished download: %s" title)
                       (kill-buffer (process-buffer process)))
                   (message "Download: [%s] failed (%d) with error: %s"
                            title (process-exit-status process) s)))))
            (message "Started download: %s" title)))
      (message "Not youtube url(s), cancelling download."))))

(defun tab-bar-switch-or-create-rss ()
  "Create or switch elfeed tab bar."
  (interactive)
  (autoload 'tab-bar-switch-or-create "lib-tabbar" nil t)
  (tab-bar-switch-or-create "Rss")
  (call-interactively #'elfeed))

(provide 'lib-elfeed)
;;; lib-elfeed.el ends here
