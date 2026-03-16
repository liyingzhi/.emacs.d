;;; init-separedit.el --- separedit                  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'separedit)

(setq separedit-default-mode 'org-mode)
(setq separedit-remove-trailing-spaces-in-comment t)
(setq separedit-continue-fill-column t)
(setq separedit-buffer-creation-hook #'auto-fill-mode)

(defun separedit/edit-org-any-block ()
  "Edit org src code block."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (when-let* ((datum (org-element-context))
                  (content-info (org-src--contents-area datum)))
        (lexical-let*
            ((ov (when (string-empty-p (nth 2 content-info))
                   (make-overlay (nth 1 content-info)
                                 (nth 1 content-info) nil nil t)))
             (ad (when ov
                   (lambda ()
                     (with-current-buffer (overlay-buffer ov)
                       (save-excursion
                         (goto-char (overlay-end ov))
                         (delete-overlay ov)
                         (unless (= (point) (point-at-bol))
                           (insert "\n")))))))
             (buf (separedit
                   (separedit-mark-region (nth 0 content-info)
                                          (nth 1 content-info)))))
          (when ad
            (with-current-buffer buf
              (setq-local kill-buffer-hook (append (list ad) kill-buffer-hook))))))
    (message "The current buffer major mode is not derived from org-mode!")))

(keymap-sets (prog-mode-map minibuffer-local-map help-mode-map)
  '(("C-c '" . separedit)))

(with-eval-after-load 'helpful
  (keymap-set helpful-mode-map "C-c '" #'separedit))

(provide 'init-separedit)
;;; init-separedit.el ends here
