(require 'multi-vterm)
(require 'vterm)

;; (add-to-list 'load-path
;;              "~/MyProject/emacs-plugin/rsync-project-mode/")

(require 'rsync-project-mode)

(setq rsync-project-default-auto-rsyncp t)
(setq rsync-project-default-gitignorep t)

(add-hook 'prog-mode-hook
          'rsync-project-mode)

(defvar project-remote-connect (make-hash-table :test #'equal))

(defun multi-vterm-project-remote ()
  "Connect project remote."
  (interactive)
  (multi-vterm-project)
  (let ((root (file-truename (project-root (project-current)))))
    (when (not (gethash root project-remote-connect))
      (puthash root t project-remote-connect)
      (when-let* ((remote-config (rsync-project-get-remote-config root))
                  (ssh-config (cl-getf remote-config :ssh-config)))
        (let ((remote-user (cl-getf ssh-config :user))
              (remote-host (cl-getf ssh-config :host))
              (remote-port (cl-getf ssh-config :port))
              (remote-path (cl-getf ssh-config :remote-dir)))
          (vterm-send-M-w)
          (vterm-send-string (format "ssh %s %s"
                                     (if (and remote-user remote-host)
                                         (format "%s@%s"
                                                 remote-user
                                                 remote-host)
                                       remote-host)
                                     (if remote-port
                                         (if (not (= 22 remote-port))
                                             (format "-p %s" remote-port)
                                           "")
                                       ""))
                             t)
          (vterm-send-return)

          (vterm-send-string (format "cd %s" remote-path))
          (vterm-send-return))))))

(transient-define-suffix rsync-project-dispatch-term()
  (interactive)
  (call-interactively #'multi-vterm-project-remote))

(transient-append-suffix 'rsync-project-dispatch '(-2 -1)
  ["Mics"
   :if rsync-project--check
   ("t" "Open Remote Term" rsync-project-dispatch-term)])

(provide 'init-rsync)
