(require 'org-agenda)
(require 'org-journal)
(require 'org-archive)
(setq org-archive-location "~/Documents/Org/archive.org::* finish-tasks")
(setq org-refile-targets '(("~/Documents/Org/archive.org" :maxlevel . 1)
                           ("~/Documents/Org/inbox.org" :maxlevel . 1)
                           ("~/Documents/Org/tasks.org" :maxlevel . 4)))
(add-list-to-list 'org-agenda-files
                  '("~/Documents/Org/idea.org"
                    "~/Documents/Org/quote.org"
                    "~/Documents/Org/tasks.org"
                    "~/Documents/Org/archive.org"
                    "~/Documents/Org/inbox.org"))

(setq org-agenda-custom-commands
      '(("e" "Agenda, next actions and waiting"
         ((agenda "" ((org-agenda-overriding-header "Next three days:")
                      (org-agenda-span 3)
                      (org-agenda-start-on-weekday nil)))
          (todo "TODO" ((org-agenda-overriding-header "Next Actions:")))
          (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))))))

;; Use a function to decide what to change the state to.
(setq org-clock-in-switch-to-state #'sodaware/switch-task-on-clock-start)

(defun sodaware/switch-task-on-clock-start (task-state)
  "Change a task to 'IN-PROGRESS' when TASK-STATE is 'TODO'."
  (if (string= task-state "TODO")
      "DOING"
    task-state))

;;; agenda repeater
;; Shorten the leaders to reserve spaces for the repeater.
(setq org-agenda-scheduled-leaders '("Sched" "S.%2dx"))
(setq org-agenda-deadline-leaders '("Deadl" "In%2dd" "D.%2dx"))

(defun my/org-agenda-repeater ()
  "The repeater shown in org-agenda-prefix for agenda."
  (if (org-before-first-heading-p)
      "-------"  ; fill the time grid
    (format "%5s: " (or (org-get-repeat) ""))))

;; Add `my/org-agenda-repeater' to the agenda prefix.
(setcdr (assoc 'agenda org-agenda-prefix-format)
        " %i %-12:c%?-12t%s%(my/org-agenda-repeater)")

;;; agennda format
(setq org-agenda-span 'day
      org-agenda-current-time-string (concat "◀┈┈┈┈┈┈┈┈┈┈┈┈┈ ⏰"))
(setq org-agenda-compact-blocks t)

(setq org-columns-default-format-for-agenda
      "%TODO %PRIORITY(Pri) %60ITEM(Task) %SCHEDULED")

;;; agenda menu
;;;###autoload
(defun agenda-open-with-file (file)
  `(lambda ()
     (interactive)
     (let ((org-agenda-files '(,file)))
       (org-agenda))))

(one-key-create-menu
 "agenda"
 `((("i" . "inbox agent") . ,(agenda-open-with-file "~/Documents/Org/inbox.org"))
   (("t" . "idea agent") . ,(agenda-open-with-file "~/Documents/Org/tasks.org"))
   (("a" . "all agent") . org-agenda)
   (("d" . "Capture roam dailies note") . org-roam-dailies-capture-today)
   (("j" . "Show open or delete journal menu") . journal-transient)))

(defconst *org-path* "~/Documents/Org/")
(setq org-roam-directory (file-truename *org-path*))
(add-list-to-list 'org-agenda-files (file-expand-wildcards (concat *org-path* "daily/*.org")))

(require 'find-lisp)
(setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))

(one-key-create-menu
 "Roam"
 '((("f" . "Find roam node") . org-roam-node-find)
   (("c" . "Capture roam node") . org-roam-capture)
   (("i" . "Insert roam node") . org-roam-node-insert)
   (("d" . "Capture roam dailies note") . org-roam-dailies-capture-today)
   (("j" . "Show open or delete journal menu") . journal-transient)))

(setq org-roam-dailies-capture-templates
      '(
        ("d" "Default" entry "** %<%H:%M> %?"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                  ("%<%a, %d %b %Y>")))
        ("w" "Weather" entry "${fetch-weather-data}"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                  ("%<%a, %d %b %Y>")))
        ("e" "Diet" entry "*** %?"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                  ("%<%a, %d %b %Y>" "Food Journal :food:")))
        ("r" "Read" entry "*** %?"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                  ("%<%a, %d %b %Y>" "What I read? :read:")))
        ("t" "Tasks" entry "*** %?"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                  ("%<%a, %d %b %Y>" "Tasks :task:")))
        ("f" "Fleeting Notes" entry "*** %?"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                  ("%<%a, %d %b %Y>" "Notes :note:")))
        ("p" "Prod" entry "** %<%H:%M> %? :prod:"
         :if-new (file+head+olp
                  "%<%Y-%m-%d>.org"
                  "#+title: %<%a, %d %b %Y>\n#+ARCHIVE: journal.org::\n"
                  ("%<%a, %d %b %Y>")))))

(defvar latitude "32.09703")
(defvar longitude "118.77969")

(defun fetch-weather-data (&rest _)
  "Fetch weather data from API and return weather string."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&daily=weather_code,temperature_2m_max,temperature_2m_min,sunrise,sunset,uv_index_max&timezone=Asia%%2FSingapore&forecast_days=1" latitude longitude)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "^$")
      (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
             (json-obj (json-read-from-string json-data))
             (daily (cdr (assoc 'daily json-obj)))
             (weather-code (aref (cdr (assoc 'weather_code daily)) 0))
             (temp-max (aref (cdr (assoc 'temperature_2m_max daily)) 0))
             (temp-min (aref (cdr (assoc 'temperature_2m_min daily)) 0))
             (sunrise (aref (cdr (assoc 'sunrise daily)) 0))
             (sunset (aref (cdr (assoc 'sunset daily)) 0))
             (uv (uv-to-sunscreen-advice (aref (cdr (assoc 'uv_index_max daily)) 0)))
             (weather-description (weather-code-to-string weather-code))
             (weather-string (format "** Weather: %s\n*** Temperature: %.1f°C-%.1f°C\n*** Daytime: %s-%s\n*** UV: %s"
                                     weather-description temp-min temp-max sunrise sunset uv)))
        weather-string))))

(defun uv-to-sunscreen-advice (uv-index)
  "Return sunscreen advice based on the given UV index."
  (let ((uv-str (number-to-string uv-index)))
    (cond
     ((<= uv-index 2) (concat uv-str " 通常不需要特别防护，但可以考虑使用SPF 15的防晒霜。"))
     ((<= uv-index 5) (concat uv-str " 建议使用SPF 15-30的防晒霜，尤其是在户外活动时。"))
     ((<= uv-index 7) (concat uv-str " 建议使用SPF 30-50的防晒霜，并采取其他防护措施，如戴帽子和太阳镜。"))
     ((<= uv-index 10) (concat uv-str " 建议使用SPF 50+的防晒霜，并尽量避免在阳光最强的时段外出，同时采取其他防护措施。"))
     ((>= uv-index 11) (concat uv-str " 强烈建议使用SPF 50+的防晒霜，并采取一切可能的防护措施，如穿长袖衣物、戴帽子和太阳镜，尽量避免暴露在阳光下。"))
     (t "输入的UV指数无效。"))))

(defun weather-code-to-string (code)
  "Convert weather CODE to a human-readable string."
  (cond
   ((= code 0) "Clear sky")
   ((= code 1) "Mainly clear")
   ((= code 2) "Partly cloudy")
   ((= code 3) "Overcast")
   ((= code 45) "Fog")
   ((= code 48) "Depositing rime fog")
   ((= code 51) "Drizzle: Light")
   ((= code 53) "Drizzle: Moderate")
   ((= code 55) "Drizzle: Dense intensity")
   ((= code 56) "Freezing Drizzle: Light")
   ((= code 57) "Freezing Drizzle: Dense intensity")
   ((= code 61) "Rain: Slight")
   ((= code 63) "Rain: Moderate")
   ((= code 65) "Rain: Heavy intensity")
   ((= code 66) "Freezing Rain: Light")
   ((= code 67) "Freezing Rain: Heavy intensity")
   ((= code 71) "Snow fall: Slight")
   ((= code 73) "Snow fall: Moderate")
   ((= code 75) "Snow fall: Heavy intensity")
   ((= code 77) "Snow grains")
   ((= code 80) "Rain showers: Slight")
   ((= code 81) "Rain showers: Moderate")
   ((= code 82) "Rain showers: Violent")
   ((= code 85) "Snow showers: Slight")
   ((= code 86) "Snow showers: Heavy")
   ((= code 95) "Thunderstorm: Slight or moderate")
   ((= code 96) "Thunderstorm with slight hail")
   ((= code 99) "Thunderstorm with heavy hail")
   (t "Unknown weather condition")))

(setq org-roam-capture-templates
      '(
        ;; #+OPTIONS: toc:nil 为了导出 .md 的格式更加符合使用
        ("d" "default" plain
         "# ------------------------------------------------------------------------------
#+title: ${title}
#+AUTHOR: Fly_lee
#+EMAIL: liyingli2018@gmail.com
#+STARTUP: content showstars indent inlineimages hideblocks
#+OPTIONS: toc:nil
# ------------------------------------------------------------------------------"
         :if-new (file "main/%<%Y%m%d%H%M%S>-${slug}.org")
         :unnarrowed t)))


(defun +delete-archived-daily-log-files ()
  "Delete Daily log files that have no titles in them."
  (interactive)
  (let ((dir (concat *org-path* "/daily/"))
        (deleted-files '()))
    (dolist (file (directory-files dir nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
      (let* ((fullpath (concat (file-name-as-directory dir) file))
             (tree (with-temp-buffer
                     (insert-file-contents fullpath)
                     (org-element-parse-buffer)))
             (headlines (org-element-map tree 'headline 'identity))
             (buffer (find-buffer-visiting fullpath)))
        (when (zerop (length headlines))
          (push file deleted-files)
          (delete-file fullpath)
          (when buffer (kill-buffer buffer)))))
    (when deleted-files
      (message "Deleted archived daily log file: %s" (string-join (nreverse deleted-files) ", ")))))

(defun journal-options (&optional args)
  "Perform various journal-related actions based on ARGS.

This function allows you to open specific journal files or perform
other journal-related actions. ARGS should be a list of arguments
that can include:

- \"journal.org\": Open the main journal file.
- \"today\": Open today's journal file.
- \"yesterday\": Open yesterday's journal file.
- \"delete\": Delete archived daily log files.

The files are located in the directory specified by `file-path-prefix`."
  (interactive (list (transient-args 'journal-transient)))
  (let ((file-path-prefix (concat *org-path* "/daily/")))
    (cond ((member "journal.org" args)
           (find-file (concat file-path-prefix (car args))))
          ((member "today" args)
           (let ((file-path (concat file-path-prefix
                                    (format-time-string "%Y-%m-%d.org"
                                                        (current-time)))))
             (if (file-exists-p file-path)
                 (find-file file-path)
               (message "Journal file not found for today"))))
          ((member "yesterday" args)
           (let ((file-path (concat file-path-prefix
                                    (format-time-string "%Y-%m-%d.org"
                                                        (time-subtract
                                                         (current-time)
                                                         (days-to-time 1))))))
             (if (file-exists-p file-path)
                 (find-file file-path)
               (message "Journal file not found for yesterday"))))
          ((member "delete" args)
           (+delete-archived-daily-log-files)))))

(transient-define-prefix journal-transient ()
  "Journal menu"
  :info-manual "Journal menu"
  ["Arguments"
   ("-j" "Journal"            "journal.org")
   ("-t" "Today"              "today")
   ("-y" "Yesterday"          "yesterday")
   ("-d" "Clear archive log"  "delete")]
  ["Commands"
   ("RET" "Journal files switch"   journal-options)]
  [("q" "Quit"           transient-quit-one)])

(org-roam-db-autosync-mode)

(provide 'init-org-agenda)
