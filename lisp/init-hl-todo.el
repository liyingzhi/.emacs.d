;;; init-hl-todo.el --- init hl todo                 -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq hl-todo-require-punctuation t
      hl-todo-highlight-punctuation ":")

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

(global-hl-todo-mode)

;;; consult-todo
(with-eval-after-load 'consult-todo
  (require 'lib-hl-todo)
  (setq consult-todo-dir-function #'consult-todo--ripgrep)
  (setq consult-todo--narrow
        (mapcar (lambda (kw)
                  (let ((key (downcase (substring (car kw) 0 1))))
                    (cons (aref key 0) (concat (car kw) hl-todo-highlight-punctuation))))
                hl-todo-keyword-faces)))

(lazy-load-global-keys
 '(("M-g t" . consult-todo-project))
 "consult-todo")

(provide 'init-hl-todo)
