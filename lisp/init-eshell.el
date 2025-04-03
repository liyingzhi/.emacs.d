(require 'eshell)
(require 'project)

;;; eshell toggle
(defun eshell-toggle--new-buffer (buf-name)
  "Init BUF-NAME."
  (let ((default-directory (project-root (project-current t)))
        (eshell-buffer-name buf-name))
    (with-temp-buffer
      (call-interactively #'eshell))))

;;;###autoload
(defun eshell-project-toggle ()
  "Show eshell at the bottom of current window and cd to current buffer's path.
Use popper manager eshell buffer."
  (interactive)
  (let ((buf-name (project-prefixed-buffer-name "eshell")))
    (if (get-buffer buf-name)
        ;; buffer is already created
        (or (-some-> buf-name get-buffer-window delete-window)
           (switch-to-buffer-other-window buf-name))
      ;; buffer is not created, create it
      (eshell-toggle--new-buffer buf-name))))

;;; eshell prompt
(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-pipeline "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-pipeline))

;;; eshell completion
(when (and (executable-find "fish")
         (require 'fish-completion nil t))
  (global-fish-completion-mode))

(when user/bash
  (setq explicit-shell-file-name "c:/Users/liyin/scoop/apps/git/current/bin/bash.exe")
  (setq shell-file-name explicit-shell-file-name)
  (add-to-list 'exec-path "c:/Users/liyin/scoop/apps/git/current/bin"))

(defun shell-toggle--new-buffer (buf-name)
  "Init BUF-NAME."
  (let ((default-directory (project-root (project-current t)))
        (shell-buffer-name buf-name))
    (with-temp-buffer
      (shell shell-buffer-name))))

(defun shell-project-toggle ()
  "Show eshell at the bottom of current window and cd to current buffer's path.
Use popper manager eshell buffer."
  (interactive)
  (let ((buf-name (project-prefixed-buffer-name "shell")))
    (if (get-buffer buf-name)
        ;; buffer is already created
        (or (-some-> buf-name get-buffer-window delete-window)
           (switch-to-buffer-other-window buf-name))
      ;; buffer is not created, create it
      (shell-toggle--new-buffer buf-name))))
(provide 'init-eshell)
