;;; init-hl-todo.el --- init hl todo                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq hl-todo-keyword-faces
      '(("TODO"   . "#00bfff")
        ("todo"   . "#00bfff")
        ("FIXME"  . "#FF0000")
        ("fixme"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("debug"  . "#A020F0")
        ("GOTCHA" . "#FF4500")
        ("gotcha" . "#FF4500")
        ("ANCHOR" . "#00ff00")
        ("anchor" . "#00ff00")
        ("NOTE"   . "#d0bf8f")
        ("note"   . "#d0bf8f")
        ("STUB"   . "#0000ff")
        ("stub"   . "#0000ff")
        ("HOLD"   . "#d0bf8f")
        ("NEXT"   . "#dca3a3")
        ("THEM"   . "#dc8cc3")
        ("DONT"   . "#5f7f5f")
        ("FAIL"   . "#8c5353")
        ("DONE"   . "#afd8af")
        ("HACK"   . "#d0bf8f")
        ("TEMP"   . "#d0bf8f")))

(setq hl-todo-require-punctuation t
      hl-todo-highlight-punctuation ":")

(global-hl-todo-mode)

;;; hl-todo search
(require 'rg)

(defun hl-todo-rg (regexp &optional files dir)
  "Use `rg' to find all TODO or similar keywords.
This function interactively prompts for file types and directory to search.
REGEXP is the regular expression to search for.
FILES is the file type pattern to limit the search.
DIR is the base directory for the search."
  (interactive
   (let ((regexp (replace-regexp-in-string "\\\\[_<>]*\\|?[0-9]+:" "" (hl-todo--setup-regexp))))
     (list regexp
           (rg-read-files)
           (read-directory-name "Base directory: " nil default-directory t))))
  (rg regexp files dir))

(defun hl-todo-rg-project (regexp &optional files dir)
  "Use `rg' to find all TODO or similar keywords in current project.
When called interactively with `C-u', prompt for file types and confirm.
Otherwise, search everything in the current project."
  (interactive
   (let ((regexp (replace-regexp-in-string "\\\\[_<>]*\\|?[0-9]+:" "" (hl-todo--setup-regexp))))
     (if current-prefix-arg
         (list regexp
               (rg-read-files)
               (project-root (project-current)))
       (list regexp "everything" (project-root (project-current))))))
  (rg regexp files dir))

(provide 'init-hl-todo)
