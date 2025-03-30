;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

(require 'project)
(require 'projection)

(defun project-dired-dir (dired-dir)
  (interactive (list
                (read-directory-name "Dired open: " (project-root (project-current)))))
  (dired dired-dir))


(lazy-one-key-create-menu
 "Project"
 (:key "f" :description "Find file in project" :command project-find-file)
 (:key "o" :description "Find other file in project" :command projection-find-other-file)
 (:key "d" :description "Project Dir" :command project-dired-dir)
 (:key "b" :description "Project buffer" :command consult-project-buffer)
 (:key "k" :description "Project kill buffer" :command project-kill-buffers)
 ;; (:key "t" :description "Open temp project" :command find-temp-project)
 (:key "p" :description "Switch project" :command project-switch-project)
 (:key "a" :description "Remember a project" :command project-remember-projects-under)
 (:key "r" :description "Remove known project" :command project-forget-project)
 (:key "v" :description "Project Git" :command magit-status)
 (:key "e" :description "Project eshell" :command eshell-project-toggle :filename "init-eshell")
 (:key "t" :description "Project shell" :command shell-project-toggle :filename "init-eshell"))

(provide 'init-project)
;;; project.el ends here
