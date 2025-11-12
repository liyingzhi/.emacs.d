;;; init-magit.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ANCHOR: magit-todos keyword text
;; (require 'magit-todos)
;; (with-eval-after-load 'magit
;;   (magit-todos-mode 1))

(defun +magit-log--abbreviate-author (&rest args)
  "The first arg is AUTHOR, abbreviate it.
First Last  -> F Last
First.Last  -> F Last
Last, First -> F Last
First       -> First (no change).

It is assumed that the author has only one or two names."
  ;; ARGS               -> '((REV AUTHOR DATE))
  ;; (car ARGS)         -> '(REV AUTHOR DATE)
  ;; (nth 1 (car ARGS)) -> AUTHOR
  (let* ((author (nth 1 (car args)))
         (author-abbr (if (string-match-p "," author)
                          ;; Last, First -> F Last
                          (replace-regexp-in-string "\\(.*?\\), *\\(.\\).*" "\\2 \\1" author)
                        ;; First Last -> F Last
                        (replace-regexp-in-string "\\(.\\).*?[. ]+\\(.*\\)" "\\1 \\2" author))))
    (setf (nth 1 (car args)) author-abbr))
  (car args))

(with-eval-after-load 'magit
  (setq magit-diff-refine-hunk t
        magit-save-repository-buffers nil
        magit-revision-insert-related-refs nil)
  (setq magit-delta-hide-plus-minus-markers nil)
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-format-file-function #'magit-format-file-nerd-icons))

(with-eval-after-load 'magit-log
  ;; Set `magit-log-margin' value in :init as many other variables will be
  ;; dynamically set based on its value when `magit-log' is loaded.
  ;; (setq magit-log-margin '(t age magit-log-margin-width t 18)) ;Default value
  ;; Show the commit ages with 1-char time units
  ;;   minute->m, hour->h, day->d, week->w, month->M, year->Y
  ;; Also reduce the author column width to 11 as the author name is being
  ;; abbreviated below.
  (setq magit-log-margin '(t age-abbreviated magit-log-margin-width :author 11))
  (advice-add 'magit-log-format-margin :filter-args #'+magit-log--abbreviate-author))

(add-hook 'magit-mode-hook
          #'(lambda ()
              (magit-wip-mode t)
              (magit-delta-mode t)))

(with-eval-after-load 'magit
  (require 'forge))

(if user/sidebar-magitblame
    ;; Make `magit-blame' always with sidebar style.
    (setq magit-blame-styles
          '((margin
             (margin-format " %s%f" " %C %a" " %H")
             (margin-width . 42)
             (margin-face . magit-blame-margin)
             (margin-body-face magit-blame-dimmed))))
  (setq magit-blame-styles
        '((headings
           (heading-format . "  %C %-18a%f %-80s  %H\n")
           (show-message . t))
          (highlight
           (highlight-face . magit-blame-highlight)))))

(defun unpackaged/open-magit-status (status-fn)
  "Use STATUS-FN Open a `magit-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (let* ((buffer-file-path (when buffer-file-name
                             (file-relative-name buffer-file-name
                                                 (locate-dominating-file buffer-file-name ".git"))))
         (section-ident `((file . ,buffer-file-path) (unstaged) (status))))
    (call-interactively status-fn)
    (when buffer-file-path
      (goto-char (point-min))
      (cl-loop until (when (equal section-ident (magit-section-ident (magit-current-section)))
                       (magit-section-show (magit-current-section))
                       (recenter)
                       t)
               do (condition-case nil
                      (magit-section-forward)
                    (error
                     (cl-return
                      (progn
                        (goto-char (point-min))
                        (magit-status-goto-initial-section)))))))))
;;;###autoload
(defun unpackaged/magit-status ()
  "Open a `magit-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (unpackaged/open-magit-status #'magit-status))

;;;###autoload
(defun unpackaged/magit-project-status ()
  "Open a `magit-project-status' buffer.
If a file was visited in the buffer that was active when this
command was called, go to its unstaged changes section."
  (interactive)
  (unpackaged/open-magit-status #'magit-project-status))

(setq magit-status-initial-section '(((unstaged) (status)) 1))

(keymap-unset magit-status-mode-map "M-n")
(keymap-unset magit-status-mode-map "M-p")


(defun my/filter-project-by-name (in-name exc-name)
  "Filter projects by name patterns.
IN-NAME is a substring to match in project names.
EXC-NAME is a substring to exclude from matching projects.
Return the first project that matches IN-NAME and does not match EXC-NAME.
If EXC-NAME is empty string, only match by IN-NAME."
  (project--ensure-read-project-list)
  (seq-find (lambda (proj)
              (and (string-match-p in-name (car proj))
                   (or (string-empty-p exc-name)
                       (not (string-match-p exc-name (car proj))))))
            project--list))

(defun my/magit-status-by-project-name (in-name exc-name)
  "Open magit-status for a project filtered by name patterns.
Prompt for IN-NAME (substring to match) and EXC-NAME (substring to exclude).
If a matching project is found, open magit-status in its directory.
Raise an error if no matching project is found."
  (interactive "sProject name (substring match): \nsExclude projects containing (leave empty to skip): ")
  (let* ((match (my/filter-project-by-name in-name exc-name)))
    (if match
        (magit-status (car match))
      (user-error "No project found containing '%s'%s" in-name
                  (if (string-empty-p exc-name)
                      ""
                    (format " and excluding '%s'" exc-name))))))

(defun my/git-pull-upstream-by-project-name (in-name exc-name)
  "Pull from upstream for a project filtered by name patterns.
Prompt for IN-NAME (substring to match) and EXC-NAME (substring to exclude).
If a matching project is found, run `vc-pull' in its directory.
Raise an error if no matching project is found."
  (interactive "sProject name (substring match): \nsExclude projects containing (leave empty to skip): ")
  (let* ((match (my/filter-project-by-name in-name exc-name)))
    (if match
        (let ((default-directory (car match)))
          (message "Pulling from upstream for project: %s" (car match))
          (vc-pull))
      (user-error "No project found containing '%s'%s" in-name
                  (if (string-empty-p exc-name)
                      ""
                    (format " and excluding '%s'" exc-name))))))

(provide 'init-magit)
;;; init-magit.el ends here
