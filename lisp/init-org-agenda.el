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

(add-list-to-list 'org-agenda-files (file-expand-wildcards (concat *org-path* "daily/*.org")))


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
   (("a" . "all agent") . org-agenda)))

(provide 'init-org-agenda)
