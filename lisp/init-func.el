;;; Look require cl package
;;;###autoload
(defun +lizqwer/check-package-use-cl ()
  "Check package is use cl."
  (interactive)
  (require 'loadhist)
  (file-dependents (feature-file 'cl)))

;;;###autoload
(defun +lizqwer/toggle-transparent ()
  (interactive)
  (if (eq (frame-parameter (selected-frame) 'alpha-background) 100)
      (let ((full-screen-state (frame-parameter nil 'fullscreen)))
        (when full-screen-state
          ;; for gnome desktop, use twice toggle-frame-fullscreen to refresh alpha-background effect
          ;; for gnome desktop, need to install Blur my Shell gnome Extension
          (call-interactively 'toggle-frame-fullscreen))
        (set-frame-parameter (selected-frame) 'alpha-background 90)
        (when full-screen-state
          (run-with-idle-timer 0.1 nil #'toggle-frame-fullscreen)))
    (set-frame-parameter (selected-frame) 'alpha-background 100)))

;;;###autoload
(defun +lizqwer/toggle-proxy ()
  (interactive)
  (if (null url-proxy-services)
      (setq url-proxy-services
            (get-url-proxy))
    (setq url-proxy-services nil)))

;;some tool function

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun tianqi ()
  "获取天气."
  (interactive)
  (eww "zh-cn.wttr.in/"))

(defun get-file-path ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

;;;###autoload
(defun +lizqwer/copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (car (last (file-name-split (get-file-path))))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
(defun +lizqwer/copy-file-path-to-clipboard ()
  "Copy the current buffer file path to the clipboard."
  (interactive)
  (let ((filepath (get-file-path)))
    (when filepath
      (kill-new filepath)
      (message "Copied buffer file path '%s' to the clipboard." filepath))))

;;;###autoload
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun copy-this-file-to (new-path)
  "Copy the current file to a new location specified by NEW-PATH and open the new file."
  (interactive
   (list (read-file-name "Copy file to: ")))
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (let ((current-file (buffer-file-name)))
    (when (file-directory-p new-path)
      (setq new-path (concat (file-name-as-directory new-path)
                             (file-name-nondirectory current-file))))
    (copy-file current-file new-path nil)
    (message "Copied '%s' to '%s'" current-file new-path)
    (find-file new-path))) ;; Open the newly created file

;;;###autoload
(defun rename-this-file (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;;###autoload
(defun browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;;###autoload
(defun +lizqwer/toggle-dark-theme ()
  "Toggle theme."
  (interactive)
  (if (cl-find user/day-theme custom-enabled-themes)
      (+lizqwer/load-theme user/night-theme)
    (+lizqwer/load-theme user/day-theme)))

;; From https://emacs.stackexchange.com/questions/5582/are-there-color-pickers-for-emacs
;;;###autoload
(defun my-insert-color-hex (&optional arg)
  "Select a color and insert its 24-bit hexadecimal RGB format.

With prefix argument \\[universal-argument] insert the 48-bit value."
  (interactive "*P")
  (let ((buf (current-buffer)))
    (list-colors-display
     nil nil `(lambda (name)
            (interactive)
            (quit-window)
            (with-current-buffer ,buf
              (insert (apply #'color-rgb-to-hex
                             (nconc (color-name-to-rgb name)
                                    (unless (consp ',arg)
                                      (list (or ,arg 2)))))))))))

;;;###autoload
(defun insert-import ()
  (interactive)
  (split-window-below)
  (other-window 1)
  (goto-char (point-min)))

;;;###autoload
(defun consult-fd-dir ()
  (interactive)
  (let ((consult-fd-args (append consult-fd-args
                                 (list
                                  "--type directory"))))
    (consult-fd "~/")))

(provide 'init-func)
