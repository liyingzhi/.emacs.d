;;; init-org-agenda.el --- org agenda                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-agenda)
(require 'org-journal)
(require 'org-archive)
(require 'init-timezones)

(setq org-archive-location (concat user/org-base-dir-path "/archive.org::* finish-tasks"))
(setq org-refile-targets `((,(concat user/org-base-dir-path "/archive.org") :maxlevel . 1)
                           (,(concat user/org-base-dir-path "/inbox.org") :maxlevel . 1)
                           (,(concat user/org-base-dir-path "/tasks.org") :maxlevel . 4)))

(add-list-to-list 'org-agenda-files
                  `(,(concat user/org-base-dir-path "/idea.org")
                    ,(concat user/org-base-dir-path "/quote.org")
                    ,(concat user/org-base-dir-path "/tasks.org")
                    ,(concat user/org-base-dir-path "/archive.org")
                    ,(concat user/org-base-dir-path "/inbox.org")))

;; (add-list-to-list 'org-agenda-files (file-expand-wildcards (concat *org-path* "daily/*.org")))

;; 计算待办事项创建至今的时间
(defun org-todo-age (&optional pos)
  "Calculate the age of an Org mode TODO entry.
If POS is given, calculate the age of the TODO entry at that position.
Otherwise, calculate the age of the current entry."
  (if-let* ((entry-age (org-todo-age-time pos))
            (days (time-to-number-of-days entry-age)))
      (cond
       ((< days 1)   "today")
       ((< days 7)   (format "%dd" days))
       ((< days 30)  (format "%.1fw" (/ days 7.0)))
       ((< days 358) (format "%.1fM" (/ days 30.0)))
       (t            (format "%.1fY" (/ days 365.0))))
    ""))

(defun org-todo-age-time (&optional pos)
  (let ((stamp (org-entry-get (or pos (point)) "TIMESTAMP_IA" t)))
    (when stamp
      (time-subtract (current-time)
                     (org-time-string-to-time stamp)))))

(keymap-set org-agenda-mode-map
            "C-c C-e" #'org-store-agenda-views)

(defun sodaware/switch-task-on-clock-start (task-state)
  "Change a task to 'IN-PROGRESS' when TASK-STATE is 'TODO'."
  (if (string= task-state "TODO")
      "DOING"
    task-state))

;; Use a function to decide what to change the state to.
(setq org-clock-in-switch-to-state #'sodaware/switch-task-on-clock-start)

;;; agenda repeater
;; Shorten the leaders to reserve spaces for the repeater.
(setq org-agenda-scheduled-leaders '("Sched:" "S.%2dx:"))
(setq org-agenda-deadline-leaders '("Deadl" "In%2dd:" "D.%2dx:"))

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
;; (setq org-agenda-compact-blocks t)

(setq org-columns-default-format-for-agenda
      "%TODO %PRIORITY(Pri) %60ITEM(Task) %SCHEDULED")

(defun prot-org-agenda-include-priority-no-timestamp ()
  "Return nil if heading has a priority but no timestamp.
Otherwise, return the buffer position from where the search should
continue, per `org-agenda-skip-function'."
  (let ((point (point)))
    (if (and (eq (nth 3 (org-heading-components)) ?A)
             (not (org-get-deadline-time point))
             (not (org-get-scheduled-time point)))
        nil
      (line-beginning-position 2))))

(defvar prot-org-custom-daily-agenda
  ;; NOTE 2021-12-08: Specifying a match like the following does not
  ;; work.
  ;;
  ;; tags-todo "+PRIORITY=\"A\""
  ;;
  ;; So we match everything and then skip entries with
  ;; `org-agenda-skip-function'.
  `((tags-todo "*"
               ((org-agenda-overriding-header "Important tasks without a date\n")
                ;; NOTE 2024-10-31: Those used to work, but now the
                ;; query for the timestamp is ignored.  I thus wrote
                ;; `prot-org-agenda-include-priority-no-timestamp'.
                ;;
                ;; (org-agenda-skip-function '(org-agenda-skip-subtree-if nil '(timestamp)))
                ;; (org-agenda-skip-function
                ;;  `(org-agenda-skip-entry-if
                ;;    'notregexp ,(format "\\[#%s\\]" (char-to-string org-priority-highest))))
                (org-agenda-skip-function #'prot-org-agenda-include-priority-no-timestamp)
                (org-agenda-block-separator nil)))
    (agenda "" ((org-agenda-overriding-header "\nPending scheduled tasks")
                (org-agenda-time-grid nil)
                (org-habit-show-habits nil)
                (org-agenda-start-on-weekday nil)
                (org-agenda-span 1)
                (org-agenda-show-all-dates nil)
                (org-scheduled-past-days 365)
                ;; Excludes today's scheduled items
                (org-scheduled-delay-days 1)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:scheduled))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "ROUTINE"))
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "")))
    (agenda "" ((org-agenda-overriding-header "\nToday's agenda\n")
                (org-agenda-span 1)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-scheduled-past-days 0)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp "ROUTINE"))
                ;; We don't need the `org-agenda-date-today'
                ;; highlight because that only has a practical
                ;; utility in multi-day views.
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")))
    ;; (agenda "" ((org-agenda-overriding-header "\nRoutine")
    ;;             (org-agenda-time-grid nil)
    ;;             (org-agenda-start-on-weekday nil)
    ;;             (org-agenda-span 1)
    ;;             (org-agenda-show-all-dates nil)
    ;;             (org-scheduled-past-days 365)
    ;;             ;; Excludes today's scheduled items
    ;;             (org-scheduled-delay-days 1)
    ;;             (org-agenda-block-separator nil)
    ;;             (org-agenda-entry-types '(:scheduled))
    ;;             (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "ROUTINE"))
    ;;             (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
    ;;             (org-agenda-format-date "")))
    (agenda "" ((org-agenda-overriding-header "\nNext three days\n")
                (org-agenda-start-on-weekday nil)
                (org-agenda-start-day nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
    (agenda "" ((org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")
                (org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done)))))
  "Custom agenda for use in `org-agenda-custom-commands'.")

(setq org-agenda-custom-commands
      `(("A" "Daily agenda and top priority tasks"
         ,prot-org-custom-daily-agenda
         ((org-agenda-fontify-priorities nil)
          ;; (org-agenda-prefix-format "	 %t %s")
          (org-agenda-dim-blocked-tasks nil)))
        ("e" "Agenda, next actions and waiting"
         ((agenda "" ((org-agenda-overriding-header "Next three days:")
                      (org-agenda-span 3)
                      (org-agenda-start-on-weekday nil)))
          (todo "TODO" ((org-agenda-overriding-header "Next Actions:")))
          (todo "WAIT" ((org-agenda-overriding-header "Waiting:")))))
        ("N" "Notes" tags "NOTE"
         ((org-agenda-overriding-header "Notes")
          (org-tags-match-list-sublevels t)))
        ("g" "GTD"
         ((agenda "" nil)
          (tags-todo "-inbox"
                     ((org-agenda-overriding-header "\nNext Actions")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-block-separator nil)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-skip-function
                       (lambda ()
                         (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                             (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-reading+PROJECT"
                     ((org-agenda-overriding-header "\nProject")
                      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                      (org-tags-match-list-sublevels t)
                      (org-agenda-block-separator nil)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "+reading+PROJECT"
                     ((org-agenda-overriding-header "\nReading")
                      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                      (org-tags-match-list-sublevels t)
                      (org-agenda-block-separator nil)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "/WAITING"
                     ((org-agenda-overriding-header "\nWaiting")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-block-separator nil)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "/DELEGATED"
                     ((org-agenda-overriding-header "\nDelegated")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-block-separator nil)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-inbox"
                     ((org-agenda-overriding-header "\nOn Hold")
                      (org-agenda-skip-function
                       (lambda ()
                         (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                             (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-block-separator nil)
                      (org-agenda-sorting-strategy
                       '(category-keep)))))
         nil
         (,(concat user/org-base-dir-path "/emacs-agenda.html") ,(concat user/org-base-dir-path "/emacs-agenda.ps")))))

;;; org-habit
(with-eval-after-load 'org-agenda
  (require 'org-habit)
  (setopt org-habit-following-days 7
          org-habit-preceding-days 7
          org-habit-show-all-today t
          org-habit-graph-column 57
          org-habit-show-done-always-green t)
  (unless user/org-latex-preview-feature
    (setopt org-habit-today-glyph ?○
            org-habit-completed-glyph ?●))

  (let ((agenda-sorting-strategy (assoc 'agenda org-agenda-sorting-strategy)))
    (setcdr agenda-sorting-strategy (remove 'habit-down (cdr agenda-sorting-strategy)))))

;;; agenda app
(when user/org-agenda-to-appt
  (add-hook 'org-agenda-finalize-hook #'org-agenda-to-appt)
  (with-hook 'after-init-hook
    (run-at-time 0 3600 'org-agenda-to-appt)
    (appt-activate t)))

;;; doing
(require 'doing)
;; Optional: change the storage directory (default: ~/org/doing/)
(setopt doing-directory "~/Documents/Org/doing/")

;; Optional: set up keybindings
(defalias 'doing-command-map doing-command-map)

(global-set-keys
 '(("C-c A d" . ("doing" . doing-command-map))))

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
   (("A" . "all agent") . org-agenda)
   (("a" . "daily agenda") . (lambda ()
                               "Call Org agenda with `prot-org-custom-daily-agenda' configuration."
                               (interactive)
                               (org-agenda nil "A")))
   (("g" . "GTD agenda") . (lambda ()
                             "Call Org agenda with GTD configuration."
                             (interactive)
                             (org-agenda nil "g")))
   (("d" . "Capture roam dailies note") . org-roam-dailies-capture-today)
   (("j" . "Show open or delete journal menu") . journal-transient)
   (("p" . "casual timezone planner") . (lambda ()
                                          (interactive)
                                          (autoload #'casual-timezone-planner "casual-timezone-utils" nil t)
                                          (autoload #'casual-timezone-settings-tmenu "casual-timezone-settings" nil t)
                                          (call-interactively #'casual-timezone-planner)))
   (("z" . "time-zones") . time-zones)))

(global-set-keys
 '(("C-c A t" . ("tmr timer" . tmr-prefix-map))))

(global-set-keys
 '(("C-c A c" . calendar)))

(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
