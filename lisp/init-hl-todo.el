(defun hl-todo-rg (regexp &optional files dir)
  "Use `rg' to find all TODO or similar keywords."
  (interactive
   (progn
     (unless (require 'rg nil t)
       (error "`rg' is not installed"))
     (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
       (list regexp
             (rg-read-files)
             (read-directory-name "Base directory: " nil default-directory t)))))
  (rg regexp files dir))

(defun hl-todo-rg-project (regexp &optional files dir)
  (interactive
   (progn
     (unless (require 'rg nil t)
       (error "`rg' is not installed"))
     (let ((regexp (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp))))
       (list regexp
             (rg-read-files)
             (project-root (project-current))))))
  (rg regexp files dir))

(setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("todo"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("fixme"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("debug"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("gotcha" . "#FF4500")
        ("ANCHOR" . "#00ff00")
        ("anchor" . "#00ff00")
        ("STUB"   . "#1E90FF")
        ("stub"   . "#1E90FF")))
(global-hl-todo-mode)

(provide 'init-hl-todo)
