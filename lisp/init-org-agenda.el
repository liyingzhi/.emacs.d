;;; init-org-agenda.el --- org agenda                -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'org-agenda)
(require 'org-journal)
(require 'org-archive)
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

(add-list-to-list 'org-agenda-files (file-expand-wildcards (concat *org-path* "daily/*.org")))

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

(setq org-agenda-custom-commands
      `(("e" "Agenda, next actions and waiting"
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
                     ((org-agenda-overriding-header "Next Actions")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-skip-function
                       (lambda ()
                         (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                            (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy
                       '(todo-state-down effort-up category-keep))))
          (tags-todo "-reading+PROJECT"
                     ((org-agenda-overriding-header "Project")
                      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "+reading+PROJECT"
                     ((org-agenda-overriding-header "Reading")
                      (org-agenda-prefix-format "%-11c%5(org-todo-age) ")
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "/WAITING"
                     ((org-agenda-overriding-header "Waiting")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "/DELEGATED"
                     ((org-agenda-overriding-header "Delegated")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled 'future)
                      (org-agenda-sorting-strategy
                       '(category-keep))))
          (tags-todo "-inbox"
                     ((org-agenda-overriding-header "On Hold")
                      (org-agenda-skip-function
                       (lambda ()
                         (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                            (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                      (org-tags-match-list-sublevels nil)
                      (org-agenda-sorting-strategy
                       '(category-keep)))))
         nil
         (,(concat user/org-base-dir-path "/emacs-agenda.html") ,(concat user/org-base-dir-path "/emacs-agenda.ps")))))

(keymap-set org-agenda-mode-map
            "C-c C-e" #'org-store-agenda-views)

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
   (("j" . "Show open or delete journal menu") . journal-transient)
   (("p" . "casual timezone planner") . (lambda ()
                                          (interactive)
                                          (autoload #'casual-timezone-planner "casual-timezone-utils" nil t)
                                          (autoload #'casual-timezone-settings-tmenu "casual-timezone-settings" nil t)
                                          (call-interactively #'casual-timezone-planner)))))



(provide 'init-org-agenda)
;;; init-org-agenda.el ends here
