;;; init-magit.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;; ANCHOR: magit-t-o-dos keyword text
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

(provide 'init-magit)
